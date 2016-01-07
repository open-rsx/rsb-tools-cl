;;;; command.lisp --- Implementation of the bridge command.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.tools.commands.bridge)

;;; `bridge' command class

(defclass bridge (rsb.tools.commands:event-queue-mixin
                  print-items:print-items-mixin)
  ((spec         :initarg  :spec
                 :type     bridge-description
                 :reader   bridge-spec
                 :documentation
                 "Stores the specification according to which the
                  bridge should be constructed and configured.")
   (self-filters :initarg  :self-filters
                 :type     list
                 :reader   bridge-self-filters
                 :documentation
                 "Stores a list of filters necessary to prevent
                  forwarding cycles that would otherwise be caused by
                  events sent by the bridge and then received by the
                  bridge."))
  (:default-initargs
   :spec (missing-required-initarg 'bridge :spec))
  (:documentation
   "Forward events from one part of a system to other parts of the system.

    When executed, the bridge command instantiates a bridge
    participant containing one or more connection participants. Each
    connection performs uni- or bidirectional forwarding of events
    between parts of the RSB system described by transports, scopes
    and filters."))

(service-provider:register-provider/class
 'rsb.tools.commands::command :bridge :class 'bridge)

(defmethod shared-initialize :around ((instance   bridge)
                                      (slot-names t)
                                      &rest args &key
                                      spec)
  ;; `check-description' signals (continuable) errors and warnings for
  ;; detected cycles and returns a list of necessary "self-filters" as
  ;; its second value.
  (let+ (((&values spec self-filters)
          (with-condition-translation
              (((error specification-error) :spec spec))
            (check-description (etypecase spec
                                 (string             (parse-spec spec))
                                 (bridge-description spec))))))
    (apply #'call-next-method
           instance slot-names
           :spec         spec
           :self-filters self-filters
           (remove-from-plist args :spec))))

(defmethod command-execute ((command bridge) &key error-policy)
  ;; `bridge-description->connection-list' transforms the list of
  ;; self-filter specifications of the form
  ;;
  ;;   (INPUT-DESCRIPTION . (OUTPUT-DESCRIPTION₁ OUTPUT-DESCRIPTION₂ …))
  ;;
  ;; into a list of specifications of the form
  ;;
  ;;   (INPUT-URI . ((OUTPUT-URI₁ . OUTPUT-ID₁) (OUTPUT-URI₂ . OUTPUT-ID₂) …))
  ;;
  ;; which the participant turns into a set of filters while creating
  ;; its child-participants.
  (let+ (((&accessors-r/o (max-queued-events command-max-queued-events)
                          (spec              bridge-spec)
                          (self-filters      bridge-self-filters))
          command)
         ((&values connections self-filters)
          (bridge-description->connection-list spec self-filters))
         (converters (rsb.tools.commands::ensure-fallback-converter)))
    (with-participant (bridge :bridge "/"
                              :converters        converters
                              :connections       connections
                              :self-filters      self-filters
                              :max-queued-events max-queued-events
                              :error-policy      error-policy)
      (rsb.patterns.bridge:pump-events bridge))))

;;; `bridge-service' command class

(defclass bridge-service (rsb.tools.commands:event-queue-mixin
                          destination-mixin
                          print-items:print-items-mixin)
  ()
  (:documentation
   "TODO"))

(service-provider:register-provider/class
 'rsb.tools.commands::command :bridge-service :class 'bridge-service)

(defmethod command-execute ((command bridge-service) &key error-policy)
  (let+ (((&structure-r/o command- destination max-queued-events) command)
         (converters (rsb.tools.commands::ensure-fallback-converter)))
    (with-participant (bridge :bridge "/"
                              :converters        converters
                              :max-queued-events max-queued-events
                              :error-policy      error-policy)
      (call-with-control-service
       destination bridge #'rsb.patterns.bridge:pump-events))))

(defun call-with-control-service (uri bridge thunk)
  (let ((uri (let+ (((&accessors (path puri:uri-path)) uri))
               (if (ends-with #\/ path)
                   uri
                   (puri:copy-uri uri :path  (concatenate 'string path "/"))))))
    (with-participants ((server   :local-server uri)
                        (informer :informer     (puri:merge-uris "state" uri)))
      (log:info "~@<Providing remote interface for ~A at ~A.~@:>" bridge uri)
      (let+ (((&flet notify (state &optional (value rsb.converter:+no-value+))
                (send informer (make-event
                                (merge-scopes (make-scope (list state))
                                              (participant-scope informer))
                                value))))
             (count  0)
             (groups (make-hash-table :test #'equal))
             (lock   (bt:make-lock "Bridge Control Service"))
             ((&flet make-group-scope (which)
                (merge-scopes (make-scope (list which))
                              (participant-scope server))))
             ((&flet+ dispose-group ((group-which . connections))
                (loop :for (which . nil) :in connections :do
                   (setf (rsb.patterns:participant-child
                          bridge which :connection)
                         nil))
                (remhash group-which groups)
                (notify "child-removed" (make-group-scope group-which)))))
        (rsb.patterns.request-reply:with-methods (server)
            (("terminate" ()
               (log:info "~@<Termination requested.~@:>")
               (bt:with-lock-held (lock)
                 (rsb.patterns.bridge:stop bridge)
                 (notify "terminated")))
             ;; Bridge operations.
             ("reset" ()
               (log:info "~@<Received request to dispose of all connection groups.~@:>")
               (bt:with-lock-held (lock)
                 (mapc #'dispose-group (hash-table-values groups)))
               rsb.converter:+no-value+)
             ("connect" (spec string)
               (log:info "~@<Received request to add a connection ~
                          group to ~A according to specification~
                          ~@:_~@:_~
                          ~@<│ ~@;~A~:>~
                          ~@:_~@:_~
                          .~:>"
                         bridge spec)
               (bt:with-lock-held (lock)
                 (let+ (;; TODO comment
                        ((&values connections self-filters)
                         (handler-bind ((forwarding-cycle-warning
                                         (lambda (condition)
                                           (log:warn "~A" condition)
                                           (muffle-warning condition)))
                                        (forwarding-cycle-error
                                         (lambda (condition)
                                           (log:warn "~A" condition)
                                           (continue condition))))
                           (multiple-value-call
                               #'bridge-description->connection-list
                             (check-description (parse-spec spec)))))
                        ((&flet+ make-connection
                             ((listeners informers filters transform))
                           (let+ (((&values connection which)
                                   (rsb.patterns:make-child-participant
                                    bridge :new :connection
                                    :listeners         listeners
                                    :informers         informers
                                    :filters           filters
                                    :transform         transform
                                    :timestamp-events? t
                                    :self-filters      self-filters)))
                             (cons which
                                   (setf (rsb.patterns:participant-child
                                          bridge which :connection)
                                         connection)))))
                        (connections   (mapcar #'make-connection connections))
                        (which         (princ-to-string (incf count)))
                        (control-scope (make-group-scope which)))
                   (setf (gethash which groups) (cons which connections))
                   (notify "child-added" control-scope)
                   (scope-string control-scope))))
             ;; Connection operations
             ("destroy" (scope scope)
               (log:info "~@<Received request to removed connection ~
                          designated by ~A.~@:>"
                         (scope-string scope))
               (bt:with-lock-held (lock)
                 (let* ((scope (make-scope scope))
                        (which (lastcar (scope-components scope)))
                        (group (or (gethash which groups)
                                   (error "~@<No such connection group: ~S.~@:>"
                                          which))))
                   (dispose-group group)))
               rsb.converter:+no-value+))

          (notify "ready")
          (unwind-protect
               (funcall thunk bridge)
            (ignore-errors (notify "terminating"))))))))

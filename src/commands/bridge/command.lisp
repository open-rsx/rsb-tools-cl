;;;; command.lisp --- Implementation of the bridge command.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.tools.commands.bridge)

;;; Construction of the `bridge' participant from a bridge
;;; specification.
;;;
;;; In this file because the method is mainly concerned with the
;;; `bridge-description' object graph about which the participant
;;; knows nothing.

(defmethod make-participant-using-class ((class     class)
                                         (prototype rsb.patterns.bridge:bridge)
                                         (scope     scope)
                                         &rest args &key
                                         spec
                                         connections
                                         self-filters)
  (let+ ((self-filters self-filters)
         ((&flet register-description-data (description data)
            (setf self-filters
                  (sublis (list (cons description data))
                          self-filters))
            data))
         ((&flet extract-input-uri (description)
            (register-description-data
             description (input-description-uri description))))
         ((&flet extract-output-uri (description)
            (let ((uri (puri:copy-uri (output-description-uri description)))
                  (id  (uuid:make-v4-uuid)))
              (register-description-data description (cons uri id)))))
         ((&flet make-filter (description)
            (apply #'rsb.filter:make-filter
                   (filter-description-class description)
                   (filter-description-initargs description))))
         ((&flet make-transform (description)
            (apply #'rsb.transform:make-transform
                   (transform-description-class description)
                   (transform-description-initargs description))))
         ((&flet process-spec (spec)
            (let+ (((&structure-r/o
                     connection-description- inputs outputs filters transform)
                    spec))
              (list (mapcar #'extract-input-uri  inputs)
                    (mapcar #'extract-output-uri outputs)
                    (mapcar #'make-filter        filters)
                    (when transform
                      (make-transform transform))))))
         (connections (append (when spec
                                (mapcar #'process-spec
                                        (bridge-description-connections spec)))
                              connections)))
    (apply #'call-next-method class prototype scope
           :connections  connections
           :self-filters self-filters
           (remove-from-plist args :spec :connections :self-filters))))

;;; `bridge' command class

(defclass bridge (print-items:print-items-mixin)
  ((spec              :initarg  :spec
                      :type     bridge-description
                      :reader   bridge-spec
                      :documentation
                      "Stores the specification according to which the
                       bridge should be constructed and configured.")
   (self-filters      :initarg  :self-filters
                      :type     list
                      :reader   bridge-self-filters
                      :documentation
                      "Stores a list of filters necessary to prevent
                       forwarding cycles that would otherwise be
                       caused by events sent by the bridge and then
                       received by the bridge.")
   (max-queued-events :initarg  :max-queued-events
                      :type     (or null positive-integer)
                      :reader   bridge-max-queued-events
                      :initform 200
                      :documentation
                      "The maximum number of events which may be
                       queued for processing at any given time. Note
                       that choosing a large value can require a large
                       amount of memory."))
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
  (let+ (((&structure-r/o bridge- spec self-filters max-queued-events) command)
         (converters (rsb.tools.commands::ensure-fallback-converter)))
    (with-participant (bridge :bridge "/"
                              :converters        converters
                              :spec              spec
                              :self-filters      self-filters
                              :max-queued-events max-queued-events
                              :error-policy      error-policy)
      (rsb.patterns.bridge:pump-events bridge))))


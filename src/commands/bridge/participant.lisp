;;;; participant.lisp --- Bridge-related specialized participants.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb.patterns.bridge
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:iterate

   #:rsb)

  (:export
   #:bridge

   #:stop
   #:pump-events))

(cl:in-package #:rsb.patterns.bridge)

;;; Utilities

(define-constant +bridge-origin-key+
    :rsb.bridge.origin :test #'eq)

(defun make-bridge-timestamp-key (id &key (which '#:received) )
  (format-symbol :keyword "~A.~A.~@:(~A~).~A" '#:rsb '#:bridge id which))

(defun make-handler (queue)
  (let ((connection nil)
        (handler    (rsb.tools.commands::make-handler queue)))
    (declare (type function handler))
    (values (lambda (event)
              (when connection
                (funcall handler (cons connection event))))
            (lambda (new-value)
              (setf connection new-value)))))

;;; `connection'

(defclass connection (rsb.patterns:composite-participant-mixin
                      rsb.patterns:child-container-mixin
                      rsb.patterns:configuration-inheritance-mixin
                      rsb:participant)
  ()
  (:documentation
   "A participant that forwards events between multiple RSB buses.

    Child participants listen on a \"source\" RSB bus (or multiple
    source buses) and collect events. Other child participants publish
    these events on a \"destination\" RSB bus (or multiple destination
    buses).

    Forwarded events can optionally be transformed when traversing the
    bridge. In addition, events can be be tagged with the identity of
    the bridge participant and the time of traversal of the bridge."))

(rsb::register-participant-class 'connection :connection)

(defmethod shared-initialize :after ((instance   connection)
                                     (slot-names t)
                                     &key
                                     listeners
                                     informers
                                     filters
                                     (timestamp-events? t)
                                     handler
                                     self-filters)
  (let+ (((&structure bridge- %queue) instance)
         ((&flet add-child (uri kind &rest args)
            (let ((name (princ-to-string uri)))
              (setf (rsb.patterns:participant-child instance name kind)
                    (apply #'rsb.patterns:make-child-participant
                           instance uri kind args)))))
         (transform (let ((initargs (rsb.patterns:make-child-initargs
                                     instance t :listener)))
                      (getf initargs :transform)))
         (read?     (or (when transform
                          (rsb.ep:access? transform :data :read))
                        (rsb.ep:access? filters :data :read)))
         (write?    (when transform
                      (rsb.ep:access? transform :data :write)))
         ((&flet annotating ()
            (rsb.tools.commands::make-annotating-converter-for-everything)))
         ((&flet components-to-drop (uri)
            (length (scope-components (rsb:uri->scope-and-options uri))))))
    ;; Create informer …
    (mapc (lambda (uri)
            (let+ (((&values uri id) (if (consp uri)
                                         (values (car uri) (cdr uri))
                                         uri)))
              (apply #'add-child uri :informer
                     (append
                      (unless (or read? write?) (list :converters (annotating)))
                      (when   id                (list :id         id))))))
          informers)
    ;; … and listener child participants (the latter potentially with
    ;; filters and transforms).
    (mapc (lambda (uri)
            (apply #'add-child uri :listener
                   :filters            filters
                   :handlers           (list handler)
                   :filter-ids         (mapcar #'cdr (cdr (assoc uri self-filters)))
                   :drop-components    (components-to-drop uri)
                   :timestamp-events?  timestamp-events?
                   (unless read? (list :converters (annotating)))))
          listeners)))

(defun make-bridge-listener-filters (ids)
  (mapcar (lambda (id)
            (let ((predicate (curry #'string= (princ-to-string id))))
              (rsb.filter:make-filter
               :complement
               :children
               (list (rsb.filter:make-filter :meta-data
                                             :key       +bridge-origin-key+
                                             :predicate predicate)))))
          ids))

(defun make-bridge-listener-transforms
    (connection-id drop-components timestamp-events?)
  (let+ ((received-timestamp-key (make-bridge-timestamp-key connection-id))
         ((&flet bridge-timestamp! (event)
            ;; TODO use the adjust-timestamps transform
            (setf (timestamp event received-timestamp-key)
                  (timestamp event :receive))
            event))
         ((&flet make-drop-scope-components (count)
            (lambda (event)
              (let* ((components (scope-components (event-scope event)))
                     (components (nthcdr count components)))
                (setf (event-scope event) (make-scope components))
                event)))))
    (append (when (typep drop-components 'positive-integer)
              (list (make-drop-scope-components drop-components)))
            (when timestamp-events?
              (list #'bridge-timestamp!)))))

(defmethod rsb.patterns:make-child-initargs ((participant connection)
                                             (which       t)
                                             (kind        (eql :listener))
                                             &key
                                             filter-ids
                                             drop-components
                                             timestamp-events?)
  (let+ ((initargs            (call-next-method))
         ;; Filter
         (filters             (getf initargs :filters))
         (bridge-filters      (make-bridge-listener-filters filter-ids))
         (effective-filters   (append bridge-filters filters))
         ;; Transform
         (transform           (getf initargs :transform))
         (bridge-transforms   (make-bridge-listener-transforms
                               (participant-id participant)
                               drop-components timestamp-events?))
         (transforms          (append bridge-transforms
                                      (when transform (list transform))))
         (effective-transform (when (<= 1 (length transforms) 3)
                                (reduce #'compose transforms))))
    (log:debug "~@<~A filters: ~:A + ~:A => ~:A~@:>"
               participant filters bridge-filters effective-filters)
    (log:debug "~@<~A transforms: ~A + ~:A => ~A~@:>"
               participant transform bridge-transforms effective-transform)
    (list* :filters   effective-filters
           :transform effective-transform
           (remove-from-plist initargs
                              :filters :filter-ids
                              :transform :drop-components :timestamp-events?))))

(defmethod rsb.patterns:make-child-initargs ((participant connection)
                                             (which       t)
                                             (kind        (eql :informer))
                                             &key
                                             id)
  (let+ ((initargs  (call-next-method))
         (transform (getf initargs :transform))
         (id/string (princ-to-string id))
         ((&flet bridge-origin! (event)
            (setf (meta-data event +bridge-origin-key+) id/string)
            event))
         (effective-transform
          (reduce #'compose (when transform (list transform))
                  :initial-value #'bridge-origin!)))
    (list* :transform effective-transform initargs)))

(defmethod send ((informer connection) (data event) &rest args &key)
  (let ((informers (remove-if-not (of-type 'informer) ; TODO cache this
                                  (rsb.patterns:participant-children informer))))
    (mapc
     (lambda (informer)
       (let ((event (apply #'make-event ; TODO copy-event
                           (merge-scopes (event-scope data) (participant-scope informer))
                           (event-data data)
                           :method            (event-method data)
                           :timestamps        (timestamp-plist data)
                           :causes            (event-causes data)
                           :create-timestamp? nil
                           (meta-data-plist data))))
         (setf (event-origin event)          (event-origin data)
               (event-sequence-number event) (event-sequence-number data))
         (apply #'send informer event :no-fill? t args)))
     informers)
    data))

;;; `bridge'

(defclass bridge (rsb.patterns:composite-participant-mixin
                  rsb.patterns:child-container-mixin
                  rsb.patterns:configuration-inheritance-mixin
                  rsb:participant)
  ((%queue :accessor bridge-%queue))
  (:documentation
   "A participant that forwards events between multiple RSB buses.

    Child participants listen to a \"source\" RSB bus (or multiple
    source buses) and collect events. Other child participants publish
    these events on a \"destination\" RSB bus (or multiple destination
    buses).

    Forwarded events can optionally be transformed when traversing the
    bridge. In addition, events can be tagged with the identity of the
    bridge participant and the time of traversal of the bridge."))

(rsb::register-participant-class 'bridge :bridge)

(defmethod shared-initialize :after ((instance   bridge)
                                     (slot-names t)
                                     &key
                                     connections
                                     self-filters
                                     (timestamp-events? t)
                                     max-queued-events)
  (let+ (((&structure bridge- %queue) instance)
         (i 0))
    (setf %queue (rsb.tools.commands::make-queue :max-queued-events max-queued-events))
    (mapc (lambda+ ((listeners informers filters transform))
            (let+ (((&values handler set-connection) (make-handler %queue))
                   (which (princ-to-string (incf i))))
              (funcall set-connection
                       (setf (rsb.patterns:participant-child
                              instance which :connection)
                             (rsb.patterns:make-child-participant
                              instance which :connection
                              :listeners          listeners
                              :informers          informers
                              :filters            filters
                              :transform          transform
                              :timestamp-events?  timestamp-events?
                              :handler            handler
                              :self-filters       self-filters)))))
          connections)))

(defmethod rsb.patterns:make-child-scope ((participant bridge)
                                          (which       t)
                                          (kind        (eql :connection)))
  (participant-scope participant))

(defgeneric stop (bridge)
  (:method ((bridge bridge))
    (lparallel.queue:push-queue :stop (bridge-%queue bridge))))

(defgeneric pump-events (bridge)
  (:method ((bridge bridge))
    ;; Process events in QUEUE until interrupted.
    (let ((queue             (bridge-%queue bridge))
          (continue-function nil))
      (restart-bind
          ((continue (lambda (&optional condition)
                       (declare (ignore condition))
                       (funcall continue-function))
             :test-function   (lambda (condition)
                                (declare (ignore condition))
                                continue-function)
             :report-function (lambda (stream)
                                (format stream "~@<Ignore the failure ~
                                                and continue ~
                                                processing.~@:>"))))
        (iter (for item next (lparallel.queue:pop-queue queue))
              (when (first-iteration-p)
                (setf continue-function (lambda () (iter:next-iteration))))
              (etypecase item
                ((eql :stop)
                 (return))
                (cons
                 (destructuring-bind (connection . event) item
                   (send connection event)))))))))

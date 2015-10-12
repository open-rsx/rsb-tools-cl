;;;; participant.lisp --- Bridge-related specialized participants.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014, 2015 Jan Moringen
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

(defun make-annotating-converter-for-everything ()
  `((t . ,(make-instance 'rsb.converter::annotating))))

(defun make-queue (&key max-queued-events)
  (apply #'lparallel.queue:make-queue
         (when max-queued-events
           (list :fixed-capacity max-queued-events))))

(defun make-handler (queue)
  (let ((connection))
    (values
     (lambda (event)
       (lparallel.queue:with-locked-queue queue
         ;; When QUEUE is full, establish a restart for flushing it
         ;; and signal an error.
         (when (lparallel.queue:queue-full-p/no-lock queue)
           (restart-case
               (error 'rsb.tools.commands::queue-overflow-error
                      :capacity (lparallel.queue:queue-count/no-lock queue)
                      :count    (lparallel.queue:queue-count/no-lock queue))
             (continue (&optional condition)
               :report (lambda (stream)
                         (format stream "~@<Flush all queued events ~
                                         and try to continue.~@:>"))
               (declare (ignore condition))
               (iter (until (lparallel.queue:queue-empty-p/no-lock queue))
                     (lparallel.queue:pop-queue/no-lock queue)))))

         ;; Potentially after flushing QUEUE, push EVENT onto it.
         (when connection
           (lparallel.queue:push-queue/no-lock (cons connection event) queue))))
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

    Child participants listen a \"source\" RSB bus (or multiple source
    buses) and collect events. Other child participants publish these
    events on a \"destination\" RSB bus (or multiple destination
    buses).

    Forwarded events can optionally transformed when traversing the
    bridge. In addition, events can be be tagged with the identity of
    the bridge participant and the time of traversal of the bridge."))

(rsb::register-participant-class 'connection :connection)

(defmethod shared-initialize :after ((instance   connection)
                                     (slot-names t)
                                     &key
                                     listeners
                                     informers
                                     filters
                                     transform
                                     (timestamp-events? t)
                                     handler
                                     self-filters)
  (let+ (((&structure bridge- %queue) instance)
         ((&flet add-child (scope kind &rest args)
            (setf (rsb.patterns:participant-child instance scope kind)
                  (apply #'rsb.patterns:make-child-participant
                         instance scope kind args))))
         (converter (make-annotating-converter-for-everything)) ; TODO not always correct: depends on filters and transforms
         ((&flet components-to-drop (uri)
            (length (scope-components (rsb:uri->scope-and-options uri))))))
    ;; Create listener and informer child participants.
    (mapc (lambda (uri)
            (let+ (((&values uri id) (if (consp uri)
                                         (values (car uri) (cdr uri))
                                         uri)))
              (apply #'add-child uri :informer
                     :converters converter
                     (when id (list :id id)))))
          informers)
    (mapc (lambda (uri)
            (add-child uri :listener
                       :filters            filters
                       :converters         converter
                       :handlers           (list handler)
                       :transform          transform
                       :filter-ids         (mapcar #'cdr (cdr (assoc uri self-filters)))
                       :drop-components    (components-to-drop uri)
                       :timestamp-events?  timestamp-events?))
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
    (setf %queue (make-queue :max-queued-events max-queued-events))
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

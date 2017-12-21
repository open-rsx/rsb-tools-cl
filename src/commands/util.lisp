;;;; util.lisp --- Utilities shared between commands.
;;;;
;;;; Copyright (C) 2015, 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.tools.commands)

;;; Scopes and URIs

(defun coerce-to-scope-or-uri (thing)
  (etypecase thing
    (string              (puri:parse-uri thing))
    ((or scope puri:uri) thing)))

(defun scope-or-uri-string (thing)
  (etypecase thing
    (puri:uri thing)
    (scope    (scope-string thing))))

(defun uri-ensure-directory-path (uri)
  (let ((path (puri:uri-path uri)))
    (if (ends-with #\/ path)
        uri
        (puri:copy-uri uri :path (concatenate 'string path "/")))))

;;; Converters

(defun ensure-fallback-converter (&key (converters (default-converters)))
  (mapcar (lambda+ ((wire-type . converter))
            (cons wire-type
                  (if (and (listp converter)
                           (not (member :fundamental-null converter)))
                      (append converter '(:fundamental-null))
                      converter)))
          converters))

(defun make-annotating-converter-for-everything ()
  `((t . ,(make-instance 'rsb.converter::annotating))))

;;; Queuing

(define-condition queue-overflow-error (rsb-error)
  ((capacity :initarg  :capacity
             :type     non-negative-integer
             :reader   queue-overflow-error-capacity
             :documentation
             "Stores the capacity of the queue in question.")
   (count    :initarg  :count
             :type     non-negative-integer
             :reader   queue-overflow-error-count
             :documentation
             "Stores the number of queued items when the queue
              overflowed."))
  (:default-initargs
   :capacity (missing-required-initarg 'queue-overflow-error :capacity)
    :count    (missing-required-initarg 'queue-overflow-error :count))
  (:report
   (lambda (condition stream)
     (format stream "~@<~:D event~:P in queue with capacity ~:D.~@:>"
             (queue-overflow-error-capacity condition)
             (queue-overflow-error-count    condition))))
  (:documentation
   "This error is signaled when an attempt is made to push an item
    onto a full fixed-capacity queue."))

(defun make-queue (&key max-queued-events)
  (apply #'lparallel.queue:make-queue
         (when max-queued-events
           (list :fixed-capacity max-queued-events))))

(defun make-handler (queue)
  (lambda (event)
    (lparallel.queue:with-locked-queue queue
      ;; When QUEUE is full, establish a restart for flushing it
      ;; and signal an error.
      (when (lparallel.queue:queue-full-p/no-lock queue)
        (restart-case
            (error 'queue-overflow-error
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
      (lparallel.queue:push-queue/no-lock event queue))))

(defun make-queue-and-handler (&key max-queued-events)
  (let ((queue (make-queue :max-queued-events max-queued-events)))
    (values queue (make-handler queue))))

;;;; util.lisp --- Utilities shared between commands.
;;;;
;;;; Copyright (C) 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.tools.commands)

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

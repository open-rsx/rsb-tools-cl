;;;; main.lisp --- Entry point of the logger tool.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.tools.logger)

(defun update-synopsis (&key
                        (show :default))
  "Create and return a commandline option tree."
  (make-synopsis
   ;; Basic usage and specific options.
   :postfix "URI*"
   :item    (make-text :contents (make-help-string :show show))
   :item    (make-common-options :show show)
   :item    (make-error-handling-options :show show)
   :item    (defgroup (:header "Logging Options"
                       :hidden (not (show-help-for?
                                     '(:logging :filters :styles :columns :quantities)
                                     :default t
                                     :show    show)))
              (stropt  :long-name       "filter"
                       :short-name      "f"
                       :argument-name   "SPEC"
                       :description
                       (make-filter-help-string :show show))
              (stropt  :long-name       "style"
                       :short-name      "s"
                       :default-value   "compact"
                       :argument-name   "SPEC"
                       :description
                       (make-style-help-string :show show))
              (lispobj :long-name      "max-queued-events"
                       :typespec       '(or null positive-integer)
                       :default-value  200
                       :argument-name  "NUMBER-OF-EVENTS"
                       :description
                       "The maximum number of events which may be queued for processing at any given time. Note that choosing a large value can require a large amount of memory."))
   :item    (make-idl-options)
   ;; Append RSB options.
   :item    (make-options
             :show? (show-help-for? :rsb :show show))
   ;; Append examples.
   :item    (defgroup (:header "Examples")
              (make-text :contents (make-examples-string)))))

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

(defun make-queue-push-handler (queue)
  (lambda (event)
    (lparallel.queue:with-locked-queue queue
      ;; When QUEUE is full, establish a restart for flushing it and
      ;; signal an error.
      (when (lparallel.queue:queue-full-p/no-lock queue)
        (restart-case
            (error 'queue-overflow-error
                   :capacity (lparallel.queue:queue-count/no-lock queue)
                   :count    (lparallel.queue:queue-count/no-lock queue))
          (continue (&optional condition)
            :report (lambda (stream)
                      (format stream "~@<Flush all queued events and ~
                                      try to continue.~@:>"))
            (declare (ignore condition))
            (iter (until (lparallel.queue:queue-empty-p/no-lock queue))
                  (lparallel.queue:pop-queue/no-lock queue)))))

      ;; Potentially after flushing QUEUE, push EVENT onto it.
      (lparallel.queue:push-queue/no-lock event queue))))

(defun make-queue-pushing-listener (handler uri error-policy filters converters)
  (let ((listener (make-listener
                   uri
                   :transports   '((t :expose (:rsb.transport.wire-schema
                                               :rsb.transport.payload-size)
                                    &inherit))
                   :converters   converters
                   :error-policy error-policy)))
    (appendf (receiver-filters listener) filters)
    (log:info "~@<Created listener ~A~@:>" listener)
    (push handler (rsb.ep:handlers listener))
    listener))

(defun process-events (queue style)
  "Process events in QUEUE until interrupted."
  (let ((continue-function nil))
    (restart-bind
        ((continue (lambda (&optional condition)
                     (declare (ignore condition))
                     (funcall continue-function))
                   :test-function   (lambda (condition)
                                      (declare (ignore condition))
                                      continue-function)
                   :report-function (lambda (stream)
                                     (format stream "~@<Ignore the ~
                                                     failure and ~
                                                     continue ~
                                                     processing.~@:>"))))
      (iter (for event next (lparallel.queue:pop-queue queue))
            (when (first-iteration-p)
              (setf continue-function (lambda () (iter:next-iteration))))
            ;; Process EVENT with STYLE.
            (format-event event style *standard-output*)))))

(defun main ()
  "Entry point function of the cl-rsb-tools-logger system."
  (update-synopsis)
  (setf *default-configuration* (options-from-default-sources))
  (process-commandline-options
   :version         (cl-rsb-tools-logger-system:version/list :commit? t)
   :update-synopsis #'update-synopsis
   :return          (lambda () (return-from main)))

  (let ((error-policy (maybe-relay-to-thread
                       (process-error-handling-options))))
    (with-print-limits (*standard-output*)
      (with-logged-warnings
        (with-error-policy (error-policy)

          ;; Load IDLs as specified on the commandline.
          (process-idl-options)

          ;; Create listeners and start the receiving and printing
          ;; loop.
          (let+ ((uris              (or (remainder) '("/")))
                 (filters           (iter (for spec next (getopt :long-name "filter"))
                                          (while spec)
                                          (collect (apply #'rsb.filter:filter
                                                          (parse-instantiation-spec spec)))))
                 (converters        (iter (for (wire-type . converter) in (default-converters))
                                          (collect
                                              (cons wire-type
                                                    (if (and (listp converter)
                                                             (not (member :fundamental-null converter)))
                                                        (append converter '(:fundamental-null))
                                                        converter)))))
                 (event-style       (make-style (parse-instantiation-spec
                                                 (getopt :long-name "style"))))
                 (max-queued-events (getopt :long-name "max-queued-events"))
                 (queue             (apply #'lparallel.queue:make-queue
                                           (when max-queued-events
                                             (list :fixed-capacity max-queued-events))))
                 (handler           (make-queue-push-handler queue)))

            (log:info "~@<Using URI~P ~:*~{~S~^, ~}~@:>" uris)
            (with-interactive-interrupt-exit ()
              (let ((listeners '()))
                (unwind-protect
                     (progn
                       (mapc (lambda (uri)
                               (push (make-queue-pushing-listener
                                      handler uri error-policy filters converters)
                                     listeners))
                             uris)
                       (process-events queue event-style))
                  (mapc #'detach/ignore-errors listeners))))))))))

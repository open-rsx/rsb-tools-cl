;;;; logger.lisp --- Implementation of the logger command.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.tools.commands)

;;; Queue and listener management

(defun make-queue-pushing-listener (handler uri error-policy filters converters)
  (let ((listener (make-participant :listener uri
                   :transports   '((t :expose (:rsb.transport.wire-schema
                                               :rsb.transport.payload-size)
                                    &inherit))
                   :converters   converters
                   :error-policy error-policy
                   :filters      filters
                   :handlers     (list handler))))
    (log:info "~@<Created listener ~A~@:>" listener)
    listener))

;;; `logger' command class

(defclass logger (source-mixin
                  event-queue-mixin
                  output-stream-mixin
                  style-mixin
                  print-items:print-items-mixin)
  ((filters :initarg  :filters
            :type     list
            :reader   logger-filters
            :initform '()
            :documentation
            "List of objects implementing the filter protocol. Only
             events accepted by all filters are processed."))
  (:default-initargs
   :filters (list *only-user-events-filter*))
  (:documentation
   "Display events as they are exchanged between RSB participants.

    Events can be filtered and displayed in several ways which can be
    controlled using the --filter and --style options.

    URIs designate the channel or channels for which events should be
    received and logged and the transport that should be used to
    attach to channel(s). If no URIs are specified, the root scope /
    and default transports are assumed.

    Use the --help-for=uri or --help-for=all options to display the
    full help text for this item."))

(service-provider:register-provider/class
 'command :logger :class 'logger)

(defun process-events (queue stream style)
  "Process events in QUEUE until interrupted."
  (let ((continue-function nil)) ; TODO there is a macro for this in rsbag
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
            (format-event event style stream)))))

(defmethod command-execute ((command logger) &key error-policy)
  (let+ (((&accessors-r/o (uris              command-uris)
                          (max-queued-events command-max-queued-events)
                          (stream            command-stream)
                          (style             command-style)
                          (filters           logger-filters))
          command)
         (converters (rsb.tools.common::maybe-ensure-idl-loading-converter
                      :converters (ensure-fallback-converter)))
         ((&values queue handler)
          (make-queue-and-handler :max-queued-events max-queued-events))
         (listeners '()))
        (unwind-protect
             (progn
               (mapc (lambda (uri)
                       (push (make-queue-pushing-listener
                              handler uri error-policy filters converters)
                             listeners))
                     uris)
               (process-events queue stream style))
          (mapc #'detach/ignore-errors listeners))))

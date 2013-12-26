;;;; call.lisp --- Implementation of the call command.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.tools.commands)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass call (destination-mixin
                  payload-literal-mixin
                  style-mixin
                  print-items:print-items-mixin)
    ((method   :initarg     :method
               :type        rsb.patterns.request-reply:method-name
               :reader      call-method
               :documentation
               "The name of the method that should be called on the
                remote server.")
     (timeout  :initarg     :timeout
               :type        (or null non-negative-real)
               :reader      call-timeout
               :initform    nil
               :documentation
               "If the result of the method call does not arrive
                within the amount of time specified by SPEC, consider
                the call to have failed and exit with non-zero
                status.")
     (no-wait? :initarg     :no-wait?
               :type        boolean
               :reader      call-no-wait?
               :initform    nil
               :documentation
               "Do not wait for the result of the method
                call. Immediately return with zero status without
                printing a result to standard output."))
    (:default-initargs
     :method (missing-required-initarg 'call :method))
    (:documentation
     "Call METHOD of the server at SERVER-URI with argument ARG.

      ARG is parsed as string when surrounded with double-quotes and
      as integer or float number when consisting of digits without and
      with decimal point respectively.

      If ARG is the single character \"-\", the entire \"contents\" of
      standard input (until end of file) is read as a string and used
      as argument for the method call.

      If ARG is the empty string, i.e. the call specification is of
      the form SERVER-URI/METHOD(), the method is called without
      argument.

      SERVER-URI designates the root scope of the remote server and
      the transport that should be used."))

  (service-provider:register-provider/class
   'command :call :class 'call))

(defmethod print-items:print-items append ((object call))
  `((:method ,(call-method object) " ~A" ((:after :destination-uri)))))

(defmethod service-provider:make-provider
    ((service  (eql (service-provider:find-service 'command)))
     (provider (eql (service-provider:find-provider 'command :call)))
     &rest args &key
     destination
     method
     (payload      nil payload-supplied?)
     (payload-spec nil payload-spec-supplied?)
     (call-spec    nil call-spec-supplied?))
  ;; Check argument compatibility.
  (when (and call-spec
             (or destination method payload-spec-supplied? payload-supplied?))
    (incompatible-initargs 'call
                           :call-spec    call-spec
                           :destination  destination
                           :method       method
                           :payload      payload
                           :payload-spec payload-spec))
  ;; If supplied, parse and translate CALL-SPEC.
  (if call-spec-supplied?
      (let+ (((&values uri method arg)
              (parse-call-spec call-spec)))
        (apply #'call-next-method service provider
               :destination uri
               :method      method
               :payload     arg
               (remove-from-plist args :call-spec)))
      (call-next-method)))

(defmethod shared-initialize :before ((instance   call)
                                      (slot-names t)
                                      &key
                                      timeout
                                      no-wait?)
  (when (and timeout no-wait?)
    (incompatible-initargs 'call
                           :timeout  timeout
                           :no-wait? no-wait?)))

(defmethod command-execute ((command call) &key error-policy)
  (let+ (((&accessors-r/o (destination command-destination)
                          (arg         command-payload)
                          (style       command-style)
                          (method      call-method)
                          (timeout     call-timeout)
                          (no-wait?    call-no-wait?))
          command)
         ((&flet call/raw (server)
            (cond
              (no-wait?
               (rsb.patterns.request-reply:call server method arg
                                                :block? nil)
               (values))
              ((not timeout)
               (rsb.patterns.request-reply:call server method arg
                                                :return :event))
              (t
               (handler-case
                   (rsb.patterns.request-reply:call server method arg
                                                    :return  :event
                                                    :timeout timeout)

                 (bt:timeout (condition)
                   (declare (ignore condition))
                   (error "~@<Method call timed out after ~S ~
                             second~:P.~@:>"
                          timeout)))))))
         ((&flet call/translate (server)
            (let ((event (call/raw server)))
              (cond
                ((not event)
                 (values))
                ((typep (event-data event) 'rsb.converter:no-value)
                 (values))
                (t
                 event))))))
    (log:info "~@<Using URI ~S method ~S arg ~A~@:>" destination method arg)
    (with-participant (server :remote-server destination
                              :error-policy error-policy)
      (when-let ((reply (multiple-value-list (call/translate server))))
        (format-event (first reply) style *standard-output*)))))

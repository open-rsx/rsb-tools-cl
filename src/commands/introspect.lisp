;;;; introspect.lisp --- Implementation of the introspect command.
;;;;
;;;; Copyright (C) 2014, 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.tools.commands)

(defclass introspect (source-mixin
                      response-timeout-mixin
                      output-stream-mixin
                      style-mixin
                      print-items:print-items-mixin)
  ((style-service :allocation :class
                  :initform 'rsb.formatting.introspection::style))
  (:documentation
   "Display information about hosts, processes and participants in a system.

    The introspection information is limited to hosts, processes and
    RSB participants reachable via the transports designated by
    URI* (zero or more URIs).

    When no URIs are supplied, the default transport configuration is
    used."))

(service-provider:register-provider/class
 'command :introspect :class 'introspect)

(defmethod command-execute ((command introspect) &key error-policy)
  (let+ (((&structure-r/o command- uris stream style response-timeout)
          command))
    (unwind-protect
         (with-participant
             (introspection
              :remote-introspection rsb.introspection:+introspection-scope+
              :receiver-uris    uris
              :error-policy     error-policy
              :change-handler   (lambda (&rest event)
                                  (rsb.ep:handle style event))
              :response-timeout response-timeout)
           (setf (rsb.formatting.introspection::style-database style)
                 (rsb.introspection::introspection-database introspection))
           (when (rsb.introspection:with-database-lock (introspection)
                   (rsb.formatting:format-event :dummy style stream))
             (sleep most-positive-fixnum)))
      (detach/ignore-errors style))))

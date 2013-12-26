;;;; introspect.lisp --- Implementation of the introspect command.
;;;;
;;;; Copyright (C) 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.tools.commands)

(defclass introspect (source-mixin
                      output-stream-mixin
                      style-mixin
                      print-items:print-items-mixin)
  ((style-service    :allocation :class
                     :initform 'rsb.formatting.introspection::style)
   (response-timeout :initarg  :response-timeout
                     :type     positive-real
                     :reader   introspect-response-timeout
                     :initform .5
                     :documentation
                     "Time in seconds to wait for responses to
                      introspection requests.

                      In most systems, all replies should arrive
                      within a few milliseconds. However,
                      circumstances like heavily loaded system,
                      degraded system performance or extreme
                      communication latency may require larger
                      values."))
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
  (let+ (((&accessors-r/o (uris             command-uris)
                          (stream           command-stream)
                          (style            command-style)
                          (response-timeout introspect-response-timeout))
          command))
    (unwind-protect
         (with-participant
             (database :remote-introspection rsb.introspection:+introspection-scope+
                       :receiver-uris    uris
                       :error-policy     error-policy
                       :change-handler   (lambda (&rest event)
                                           (rsb.ep:handle style event))
                       :response-timeout response-timeout)
           (setf (rsb.formatting.introspection::style-database style)
                 database)
           (when (rsb.formatting:format-event :dummy style stream)
             (sleep most-positive-fixnum)))
      (detach/ignore-errors style))))

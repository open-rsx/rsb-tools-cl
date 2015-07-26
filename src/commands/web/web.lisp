;;;; web.lisp --- Serve system information via HTTP.
;;;;
;;;; Copyright (C) 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.tools.commands.web)

(defclass web (source-mixin
               response-timeout-mixin
               http-server-mixin
               print-items:print-items-mixin)
  ()
  (:documentation
   "Serve information about an RSB system via HTTP.

    Introspection information is limited to hosts, processes and RSB
    participants reachable via the transports designated by URI* (zero
    or more URIs).

    When no URIs are supplied, the default transport configuration is
    used."))

(service-provider:register-provider/class
 'rsb.tools.commands::command :web :class 'web)

(defvar *database*)

(defmethod command-register-handlers ((command web))
  (mapc (curry #'apply #'command-register-handler command)
        (list (list "/introspection/json"
                    (make-instance 'introspection-json-handler
                                   :database *database*)))))

(defmethod command-execute ((command web) &key error-policy)
  (let+ (((&structure
           command- uris response-timeout address port static-directory
           (acceptor %acceptor) handlers)
          command))
    (with-participant
        (*database* :remote-introspection rsb.introspection:+introspection-scope+
                    :receiver-uris    uris
                    :error-policy     error-policy
                    :response-timeout response-timeout)
      (call-next-method))))
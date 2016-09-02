;;;; command.lisp --- Serve system information via HTTP.
;;;;
;;;; Copyright (C) 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.tools.commands.web)

(defvar *default-handlers* '()
  "Stores a list of functions that return handlers and corresponding
   paths.")

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

(defmethod command-make-handlers ((command web))
  (mapcar (lambda (generator)
            (multiple-value-call #'cons (funcall generator *database*)))
          *default-handlers*))

(defmethod command-execute ((command web) &key error-policy)
  (let+ (((&structure command- uris response-timeout) command))
    (with-participant
        (*database* :remote-introspection rsb.introspection:+introspection-scope+
                    :receiver-uris    uris
                    :error-policy     error-policy
                    :response-timeout response-timeout)
      (call-next-method))))

;;;; Protocol.lisp --- Protocol provided by the tools.commands.web module.
;;;;
;;;; Copyright (C) 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.tools.commands.web)

(defgeneric command-make-handlers (command)
  (:documentation
   "COMMAND makes and returns a list of handlers with elements of the
    form

      (PATH . HANDLER)

    ."))

(defgeneric command-register-handler (command acceptor path handler)
  (:documentation
   "COMMAND registers HANDLER under PATH in with ACCEPTOR.

    If PATH is a string, it corresponds to the prefix of URI paths
    that should be handled by HANDLER. If PATH is a function, it has
    to, when called with a `request' instance, return a Boolean
    indicating whether HANDLER should handle the request.

    HANDLER is a function of one argument, a `hunchentoot:request',
    that should be called to handle requests arriving for PATH."))

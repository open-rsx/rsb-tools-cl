;;;; Protocol.lisp --- Protocol provided by the tools.commands.web module.
;;;;
;;;; Copyright (C) 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.tools.commands.web)

(defgeneric command-register-handler (command path handler)
  (:documentation
   "Register HANDLER under PATH in COMMAND.

    PATH is a string, corresponding to the prefix of URI paths that
    should be handled by HANDLER.

    HANDLER is a function of no arguments that should be called to
    handle requests arriving for PATH."))

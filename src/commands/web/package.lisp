;;;; package.lisp --- Package definition for the commands.web module.
;;;;
;;;; Copyright (C) 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb.tools.commands.web
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:more-conditions

   #:rsb
   #:rsb.tools.commands)

  (:import-from #:rsb.tools.commands
   #:response-timeout-mixin
   #:command-response-timeout)

  ;; Conditions
  (:export
   #:argument-condition
   #:argument-condition-parameter

   #:argument-error

   #:argument-type-error

   #:argument-parse-error
   #:argument-parse-error-raw-value)

  ;; Web command protocol
  (:export
   #:command-maker-handlers

   #:command-register-handler)

  ;; Resource protocol
  (:export
   #:find-resource
   #:map-resources)

  (:documentation
   "Package definition for the commands.web module."))

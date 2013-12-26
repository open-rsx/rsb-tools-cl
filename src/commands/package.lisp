;;;; package.lisp --- Package definition for the commands module.
;;;;
;;;; Copyright (C) 2013, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb.tools.commands
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:iterate
   #:more-conditions

   #:rsb
   #:rsb.common
   #:rsb.formatting)

  (:shadow
   #:call-method)

  ;; Command protocol
  (:export
   #:command-execute)

  ;; Command creation protocol
  (:export
   #:make-command)

  (:documentation
   "Package definition for the commands module."))

;;;; package.lisp --- Package definition for the call utility.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb.tools.call
  (:use
   #:cl
   #:alexandria
   #:iterate
   #:let-plus

   #:rsb
   #:rsb.patterns.request-reply

   #:rsb.common
   #:rsb.formatting

   #:rsb.tools.commands

   #:com.dvlsoft.clon)

  (:export
   #:main)

  (:documentation
   "Main package of the `cl-rsb-tools-call' system."))

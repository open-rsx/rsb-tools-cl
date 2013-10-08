;;;; package.lisp --- Package definition for rsb-logger module.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb.tools.logger
  (:use
   #:cl
   #:alexandria
   #:iterate
   #:let-plus

   #:rsb
   #:rsb.common
   #:rsb.formatting

   #:com.dvlsoft.clon)

  (:export
   #:main)

  (:documentation
   "Main package of the `cl-rsb-tools-logger' system."))

;;;; package.lisp --- Package definition for rsb-introspect module.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb.tools.introspect
  (:use
   #:cl
   #:alexandria
   #:let-plus

   #:rsb
   #:rsb.common

   #:com.dvlsoft.clon)

  (:export
   #:main)

  (:documentation
   "Main package of the `cl-rsb-tools-introspect' system."))

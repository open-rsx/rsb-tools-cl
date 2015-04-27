;;;; package.lisp --- Package definition for rsb-introspect module.
;;;;
;;;; Copyright (C) 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb.tools.web
  (:use
   #:cl
   #:alexandria
   #:let-plus

   #:rsb
   #:rsb.common

   #:rsb.tools.commands

   #:com.dvlsoft.clon)

  (:export
   #:main)

  (:documentation
   "Main package of the `cl-rsb-tools-web' system."))

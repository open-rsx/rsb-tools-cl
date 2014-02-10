;;;; package.lisp --- Package definition for rsb-introspect module.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb.tools.introspect
  (:use
   #:cl
   #:alexandria
   #:iterate
   #:let-plus
   #:more-conditions

   #:rsb
   #:rsb.introspection
   #:rsb.common

   #:com.dvlsoft.clon)

  (:import-from #:rsb.introspection ; for printing
   #:remote-introspection-database

   #:remote-introspection
   #:introspection-database)

  (:export
   #:main)

  (:documentation
   "Main package of the `cl-rsb-tools-introspect' system."))

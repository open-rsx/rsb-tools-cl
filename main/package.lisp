;;;; package.lisp --- Package definition for the main rsb tools program.
;;;;
;;;; Copyright (C) 2011, 2013, 2014, 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb.tools.main
  (:use
   #:cl
   #:alexandria
   #:let-plus

   #:net.didierverna.clon

   #:rsb.tools.commands)

  (:export
   #:main)

  (:documentation
   "Package definition for the main rsb tools program."))

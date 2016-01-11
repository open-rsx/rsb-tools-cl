;;;; package.lisp --- Package definition for rsb-bridge module.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014, 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb.tools.bridge
  (:use
   #:cl
   #:alexandria
   #:iterate
   #:let-plus
   #:more-conditions

   #:rsb

   #:rsb.tools.common
   #:rsb.tools.commands

   #:net.didierverna.clon)

  (:export
   #:main
   #:main/service)

  (:documentation
   "Main package of the `cl-rsb-tools-bridge' system."))

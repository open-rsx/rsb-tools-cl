;;;; package.lisp --- Package definition for the send utility.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb.tools.send
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:iterate

   #:rsb
   #:rsb.common

   #:com.dvlsoft.clon)

  (:export
   #:main)

  (:documentation
   "Main package of the `cl-rsb-tools-send' system."))

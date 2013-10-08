;;;; package.lisp --- Package definition for the main rsb tools program.
;;;;
;;;; Copyright (C) 2011, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb.tools.main
  (:use
   #:cl
   #:alexandria
   #:com.dvlsoft.clon)

  (:export
   #:main)

  (:export
   #:make-static
   #:make-dynamic)

  (:documentation
   "Package definition for the main rsb tools program."))

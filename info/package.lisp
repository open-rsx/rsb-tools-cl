;;;; package.lisp --- Package definition for the info utility.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage :rsb.tools.info
  (:use
   :cl
   :alexandria
   :let-plus

   :rsb
   :rsb.common
   :rsb.formatting

   :com.dvlsoft.clon)

  (:export
   :main)

  (:documentation
   "Main package of the `cl-rsb-tools-info' system."))

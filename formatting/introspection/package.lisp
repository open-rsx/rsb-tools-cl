;;;; package.lisp --- Package definition for the formatting.introspection module.
;;;;
;;;; Copyright (C) 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb.formatting.introspection
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:iterate

   #:rsb
   #:rsb.model
   #:rsb.introspection)

  (:import-from #:rsb.introspection ; for printing
   #:tracked-quantity
   #:tracked-quantity-value
   #:tracked-quantity-history

   #:timing-tracker-%latency
   #:timing-tracker-%clock-offset

   #:entry-%tracker

   #:remote-introspection-database

   #:remote-introspection
   #:introspection-database)

  (:documentation
   "This package contains formatting functions for introspection
    information."))

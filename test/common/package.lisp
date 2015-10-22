;;;; package.lisp --- Package definition for unit tests of the cl-rsb-common system.
;;;;
;;;; Copyright (C) 2012, 2013, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb.common.test
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:lift

   #:rsb.common)

  (:export
   #:common-root)

  (:documentation
   "This package contains unit tests for the cl-rsb-common system"))

(cl:in-package #:rsb.common.test)

(deftestsuite common-root ()
  ()
  (:timeout 20)
  (:documentation
   "Root unit test suite for the cl-rsb-common system."))

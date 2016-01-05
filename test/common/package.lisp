;;;; package.lisp --- Package definition for unit tests of the rsb-tools-common system.
;;;;
;;;; Copyright (C) 2012, 2013, 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb.tools.common.test
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:lift

   #:rsb.tools.common)

  (:export
   #:common-root)

  (:documentation
   "This package contains unit tests for the rsb-tools-common system"))

(cl:in-package #:rsb.tools.common.test)

(deftestsuite common-root ()
  ()
  (:timeout 20)
  (:documentation
   "Root unit test suite for the rsb-tools-common system."))

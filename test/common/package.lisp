;;;; package.lisp --- Package definition for unit tests of the cl-rsb-common system.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage :rsb.common.test
  (:use
   :cl
   :let-plus
   :lift

   :rsb.common)

  (:export
   :common-root)

  (:documentation
   "This package contains unit tests for the cl-rsb-common system"))

(cl:in-package :rsb.common.test)

(deftestsuite common-root ()
  ()
  (:documentation
   "Root unit test suite for the cl-rsb-common system."))

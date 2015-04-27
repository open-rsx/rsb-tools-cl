;;;; package.lisp --- Package definition for unit tests of the formatting.introspection module.
;;;;
;;;; Copyright (C) 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb.formatting.introspection.test
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:more-conditions
   #:lift

   #:rsb
   #:rsb.formatting)

  (:export
   #:formatting.introspection-root)

  (:documentation
   "This package contains unit tests for the formatting module."))

(cl:in-package #:rsb.formatting.introspection.test)

(deftestsuite rsb.formatting.introspection-root (formatting-root)
  ()
  (:documentation
   "Root unit test suite for the formatting.introspection module."))

;;;; package.lisp --- Package definition for unit tests of the commands module.
;;;;
;;;; Copyright (C) 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb.tools.commands.test
  (:use
   #:cl
   #:let-plus
   #:more-conditions

   #:lift

   #:rsb.tools.commands)

  (:export
   #:commands-root)

  (:documentation
   "This package contains unit tests for the commands module."))

(cl:in-package #:rsb.tools.commands.test)

(deftestsuite commands-root ()
  ()
  (:documentation
   "Root unit test suite for the commands module."))

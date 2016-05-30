;;;; package.lisp --- Package definition for unit tests of the commands module.
;;;;
;;;; Copyright (C) 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb.tools.commands.test
  (:use
   #:cl
   #:alexandria
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
  (:timeout 20)
  (:documentation
   "Root unit test suite for the commands module."))

(defvar *safe-configuration*
  '(((:introspection :enabled)        . "0")
    ((:transport :inprocess :enabled) . "1")
    ((:transport :socket :enabled)    . "0")))

;;;; package.lisp --- Package definition for unit tests of the tools.commands.bridge module.
;;;;
;;;; Copyright (C) 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb.tools.commands.bridge.test
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:more-conditions

   #:lift

   #:rsb.tools.commands
   #:rsb.tools.commands.bridge)

  (:import-from #:rsb.tools.commands.test
   #:commands-root

   #:*safe-configuration*

   #:with-asynchronously-executing-command)

  (:export
   #:commands-bridge-root)

  (:documentation
   "This package contains unit tests for the commands.bridge
    module."))

(cl:in-package #:rsb.tools.commands.bridge.test)

(deftestsuite commands-bridge-root (commands-root)
  ()
  (:documentation
   "Root unit test suite for the tools.commands.bridge module."))

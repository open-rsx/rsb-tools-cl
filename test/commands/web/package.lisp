;;;; package.lisp --- Package definition for unit tests of the tools.commands.web module.
;;;;
;;;; Copyright (C) 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb.tools.commands.web.test
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:more-conditions

   #:lift

   #:rsb.tools.commands
   #:rsb.tools.commands.web)

  (:import-from #:rsb.tools.commands.test
   #:commands-root

   #:*safe-configuration*)

  (:export
   #:commands-web-root)

  (:documentation
   "This package contains unit tests for the commands.web module."))

(cl:in-package #:rsb.tools.commands.web.test)

(deftestsuite commands-web-root (commands-root)
  ()
  (:documentation
   "Root unit test suite for the tools.commands.web module."))

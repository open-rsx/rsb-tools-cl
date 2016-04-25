;;;; package.lisp --- Package definition for common module.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014, 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb.tools.common
  (:use
   #:cl
   #:alexandria
   #:split-sequence
   #:let-plus
   #:iterate
   #:more-conditions

   #:net.didierverna.clon

   #:rsb)

  (:local-nicknames
   (#:bp #:architecture.builder-protocol))

  ;; Conditions
  (:export
   #:call-specification-error
   #:call-specification-error-specification

   #:failed-to-load-idl
   #:failed-to-load-idl-source)

  ;; Variables
  (:export
   #:*info-output*

   #:*only-user-events-filter*)

  ;; Error handling
  (:export
   #:abort/signal
   #:continue/verbose

   #:maybe-relay-to-thread

   #:call-with-error-policy
   #:with-error-policy)

  ;; IDL loading
  (:export
   #:load-idl

   #:find-and-load-idl
   #:ensure-idl-loaded)

  ;; Logging
  (:export
   #:with-logged-warnings)

  (:export
   #:parse-payload-spec

   #:parse-call-spec

   #:parse-meta-data
   #:parse-timestamp
   #:parse-cause)

  ;; Commandline options
  (:export
   #:make-common-options
   #:process-commandline-options

   #:make-error-handling-options
   #:process-error-handling-options

   #:make-idl-options
   #:process-idl-options

   #:parse-instantiation-spec)

  ;; Interactive stuff
  (:export
   #:with-interactive-interrupt-exit)

  ;; Help text generation
  (:export
   #:show-help-for?
   #:with-abbreviation

   #:print-uri-help
   #:print-filter-help

   #:print-version

   #:print-classes-help-string

   #:first-line-or-less)

  ;; Debugging
  (:export
   #:trace-things
   #:disable-debugger

   #:start-swank
   #:enable-swank-on-signal)

  (:documentation
   "This package contains some common utility functions for RSB:
    + Commandline option definition and processing
    + Help text generation
    + Debugger control"))

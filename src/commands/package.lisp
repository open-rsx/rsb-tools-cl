;;;; package.lisp --- Package definition for the commands module.
;;;;
;;;; Copyright (C) 2013, 2014, 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb.tools.commands
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:iterate
   #:more-conditions

   #:rsb

   #:rsb.tools.common
   #:rsb.formatting)

  (:shadow
   #:call-method)

  ;; Utilities
  (:export
   #:coerce-to-scope-or-uri
   #:scope-or-uri-string

   #:uri-ensure-directory-path)

  ;; Command protocol
  (:export
   #:command-execute)

  ;; Command creation protocol
  (:export
   #:make-command)

  ;; Command mixin classes
  (:export
   #:output-stream-mixin
   #:command-stream

   #:source-mixin
   #:command-uris

   #:destination-mixin
   #:command-destination

   #:payload-literal-mixin
   #:command-payload

   #:style-mixin
   #:command-style

   #:event-queue-mixin
   #:command-max-queued-events)

  ;; General purpose command classes
  (:export
   #:redump
   #:redump-output-file
   #:redump-static?
   #:redump-compression)

  (:documentation
   "Package definition for the commands module."))

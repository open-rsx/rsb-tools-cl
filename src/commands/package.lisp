;;;; package.lisp --- Package definition for the commands module.
;;;;
;;;; Copyright (C) 2013, 2014, 2015, 2016, 2017 Jan Moringen
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

  ;; Command style protocol and mixin
  (:export
   #:command-style
   #:command-style-service
   #:command-make-style

   #:style-mixin)

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

   #:event-queue-mixin
   #:command-max-queued-events

   #:filter-mixin
   #:command-filters)

  ;; General purpose command classes
  (:export
   #:redump
   #:redump-output-file
   #:redump-static?
   #:redump-compression)

  (:documentation
   "Package definition for the commands module."))

;;;; package.lisp --- Package definition for implementation of the bridge command.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb.tools.commands.bridge
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:iterate
   #:more-conditions

   #:rsb

   #:rsb.common
   #:rsb.tools.commands)

  (:local-nicknames
   (#:bp #:architecture.builder-protocol))

  (:import-from #:esrap
   #:defrule
   #:? #:&)

  (:import-from #:rsb.model.inference
   #:communication?)

  ;; Conditions
  (:export
   #:specification-condition
   #:specification-condition-spec

   #:specification-error

   #:forwarding-cycle-condition
   #:forwarding-cycle-condition-source
   #:forwarding-cycle-condition-destination

   #:forwarding-cycle-warning

   #:forwarding-cycle-error)

  (:export
   #:parse-spec)

  (:documentation
   "This package contains the implementation of the bridge command."))

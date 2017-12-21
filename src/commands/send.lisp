;;;; send.lisp --- Entry point of the send tool.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.tools.commands)

(defclass send (destination-mixin
                payload-literal-mixin
                print-items:print-items-mixin)
  ((method     :initarg  :method
               :type     (or null string)
               :reader   send-method
               :documentation
               "Set the method field of the event being sent to
                METHOD. Default behavior is sending an event without
                method field.")
   (meta-data  :initarg  :meta-data
               :type     list #| of (cons keyword (cons string null)) |#
               :reader   send-meta-data
               :documentation
               "Set the meta-data item NAME to VALUE in the event being
                sent. This option can be specified multiple times for
                distinct NAMEs."
               ;; argument-spec NAME=VALUE
               )
   (timestamps :initarg  :timestamps
               :initarg  :timestamp
               :type     list #| of local-time:timestamp |#
               :reader   send-timestamps
               :documentation
               "Set the timestamp named NAME to the timestamp
                YYYY-MM-DD[THH:MM:SS[.µµµµµµ[+ZH:ZM]]] in the event
                being sent. This option can be specified multiple times
                for distinct NAMEs."
               ;; argument-spec NAME=YYYY-MM-DD[THH:MM:SS[.µµµµµµ[+ZH:ZM]]]
               )
   (causes     :initarg  :causes
               :initarg  :cause
               :type     list #| of event-id |#
               :reader   send-causes
               :documentation
               "Add the event id described by
                PARTICIPANT-ID:SEQUENCE-NUMBER to the cause vector of
                the event being sent. This option can be specified
                multiple times."
               ;; argument-spec "PARTICIPANT-ID:SEQUENCE-NUMBER"
               ))
  (:documentation
   "Send an event constructed according to EVENT-SPEC to listeners on
    scopes specified by DESTINATION-URI.

    EVENT-SPEC is parsed as string when surrounded with double-quotes
    and as integer or float number when consisting of digits without
    and with decimal point respectively.

    If EVENT-SPEC is the single character \"-\", the entire
    \"contents\" of standard input (until end of file) is read as a
    string and used as argument for the method send.

    DESTINATION-URI designates the destination scope to which the
    event should be sent and the transport configuration which should
    be used for sending the event."))

(service-provider:register-provider/class
 'command :send :class 'send)

(defmethod command-execute ((command send) &key error-policy)
  (let+ (((&accessors-r/o (destination command-destination)
                          (payload     command-payload)
                          (method      send-method)
                          (meta-data   send-meta-data)
                          (timestamps  send-timestamps)
                          (causes      send-causes))
          command)
         (converters (rsb.tools.common::maybe-ensure-idl-loading-converter)))
    (with-participant (informer :informer destination
                                :error-policy error-policy
                                :converters   converters)
      (apply #'send informer payload
             (append (when method     (list :method     method))
                     (when timestamps (list :timestamps timestamps))
                     (when causes     (list :causes     causes))
                     meta-data)))))

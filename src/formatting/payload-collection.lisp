;;;; payload-collections.lisp --- Format event collections.
;;;;
;;;; Copyright (C) 2012, 2013, 2014, 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.formatting)

(defvar *by-scope-formatting-converter*
  (append (ensure-list (rsb:default-converter 'octet-vector))
          '(:fundamental-null))
  "The converter that should be applied to inner payloads in event
   collection payloads.")

(defvar *by-scope-formatting-event-style*
  (make-style :detailed :separator nil)
  "The formatting style used for sub-events contained in collections
   of events.")

(defmethod format-payload ((data   rsb.protocol:notification)
                           (style  payload-style-generic/pretty)
                           (stream stream)
                           &key)
  ;; TODO(jmoringe, 2012-02-05): there should be a dedicated serialization
  (handler-case
      (let ((event (rsb.transport.socket::notification->event*
                    *by-scope-formatting-converter* data
                    :expose-wire-schema? t)))
        (pprint-logical-block (stream (list event))
          (format-event event *by-scope-formatting-event-style* stream)))
    (error (condition)
      (format stream "~@<| ~@;~
                        Failed to decode notification~
                        ~@:_~@:_~
                        ~2@T~<~A~:>~
                        ~@:_~@:_~
                        ~A~
                      ~:>"
              (list (with-output-to-string (stream)
                      (describe data stream)))
              condition))))

(defmethod format-payload ((data   rsb.protocol.collections:events-by-scope-map/scope-set)
                           (style  payload-style-generic/pretty)
                           (stream stream)
                           &key)
  (let+ (((&accessors-r/o
           (scope         rsb.protocol.collections:events-by-scope-map/scope-set-scope)
           (notifications rsb.protocol.collections:events-by-scope-map/scope-set-notifications))
          data))
    (format stream "Scope ~S~@:_~2@T"
            (sb-ext:octets-to-string scope))
    (pprint-logical-block (stream (coerce notifications 'list))
      (iter (for notification in-sequence notifications)
            (unless (first-iteration-p)
              (pprint-newline :mandatory stream))
            (format-payload notification style stream)))))

(defmethod format-payload ((data   rsb.protocol.collections:events-by-scope-map)
                           (style  payload-style-generic/pretty)
                           (stream stream)
                           &key)
  (let ((sets (rsb.protocol.collections:events-by-scope-map-sets data)))
    (format stream "Events by Scope (~D Scope~:P)~@:_~2@T"
            (length sets))
    (pprint-logical-block (stream (coerce sets 'list))
      (iter (for set in-sequence sets)
            (unless (first-iteration-p)
              (pprint-newline :mandatory stream))
            (format-payload set style stream)))))

;;;; payload-collections.lisp --- Format event collections.
;;;;
;;;; Copyright (C) 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.formatting)

;; TODO(jmoringe, 2012-02-06): hack
#.(asdf:load-system :usocket)

(defvar *by-scope-formatting-converter*
  (append (ensure-list (rsb:default-converter 'octet-vector))
          '(:fundamental-null))
  "The converter that should be applied to inner payloads in event
   collection payloads.")

(defvar *by-scope-formatting-event-style*
  (make-instance (find-style-class :detailed) :separator nil)
  "The formatting style used for sub-events contained in collections
   of events.")

(defmethod format-payload ((data   rsb.protocol:notification)
                           (style  t)
                           (stream stream)
                           &key &allow-other-keys)
  ;; TODO(jmoringe, 2012-02-05): there should be a dedicated serialization
  (handler-case
      (let ((event (rsb.transport.socket::notification->event*
                    *by-scope-formatting-converter* data
                    :expose-wire-schema? t)))
        (format-event event *by-scope-formatting-event-style* stream))
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
                           (style  t)
                           (stream stream)
                           &key &allow-other-keys)
  (let+ (((&accessors-r/o
           (scope         rsb.protocol.collections:events-by-scope-map/scope-set-scope)
           (notifications rsb.protocol.collections:events-by-scope-map/scope-set-notifications))
          data))
    (with-indented-section (stream (format nil "Scope ~S"
                                           (sb-ext:octets-to-string scope))
                                   :final-fresh-line? nil)
      (iter (for notification in-sequence notifications)
            (unless (first-iteration-p)
              (fresh-line stream))
            (format-payload notification style stream)))))

(defmethod format-payload ((data   rsb.protocol.collections:events-by-scope-map)
                           (style  t)
                           (stream stream)
                           &key &allow-other-keys)
  (let ((sets (rsb.protocol.collections:events-by-scope-map-sets data)))
    (with-indented-section (stream (format nil "Events by Scope (~D Scope~:P)"
                                           (length sets))
                                   :final-fresh-line? nil)
      (iter (for set in-sequence sets)
            (unless (first-iteration-p)
              (fresh-line stream))
            (format-payload set style stream)))))

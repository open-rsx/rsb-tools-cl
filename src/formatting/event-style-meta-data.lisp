;;;; event-style-meta-data.lisp --- Meta-data-only formatting style class.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.formatting)

(defclass style-meta-data (separator-mixin)
  ((routing-info? :initarg  :routing-info?
                  :type     boolean
                  :accessor style-routing-info?
                  :initform t
                  :documentation
                  "Should routing information like destination scope,
                   origin id and method be printed?")
   (timestamps?   :initarg  :timestamps?
                  :type     boolean
                  :accessor style-timestamps?
                  :initform t
                  :documentation
                  "Should event timestamps be printed?")
   (user-items?   :initarg  :user-items?
                  :type     boolean
                  :accessor style-user-items?
                  :initform t
                  :documentation
                  "Should the dictionary of client-supplied key-value
                   pairs be printed?")
   (causes?       :initarg  :causes?
                  :type     boolean
                  :accessor style-causes?
                  :initform t
                  :documentation
                  "Should the causes of the event be printed?"))
  (:default-initargs
   :separator `((:rule ,(if *textual-output-can-use-utf-8?* #\─ #\-))
                #\Newline))
  (:documentation
   "Format the meta-data of each event on multiple lines, but do not
    format event payloads."))

(service-provider:register-provider/class
 'style :meta-data :class 'style-meta-data)

(defmethod format-event ((event  event)
                         (style  style-meta-data)
                         (stream t)
                         &key
                         (max-lines 20)
                         &allow-other-keys)
  (let+ (((&structure-r/o style- routing-info? timestamps? user-items? causes?)
          style)
         (*print-lines* max-lines)      ; TODO avoid?
         (produced-output? nil))
    (when (not (or routing-info? timestamps? user-items? causes?))
      (return-from format-event))

    (pprint-logical-block (stream (list event))

      ;; Envelope information.
      (when routing-info?
        (let+ (((&structure-r/o event- scope id sequence-number origin method)
                event))
          (format stream "Event~
                          ~@:_~2@T~@<~
                            Scope           ~A~@:_~
                            Id              ~:[<none>~;~:*~A~]~@:_~
                            Sequence number ~:[<none>~;~:*~:D~]~@:_~
                            Origin          ~:[<none>~;~:*~A~]~@:_~
                            Method          ~:[<none>~;~:*~A~]~
                          ~:>"
                  (scope-string scope) id sequence-number origin method))
        (setf produced-output? t))

      ;; Framework and user timestamps.
      (when timestamps?
        (let+ (((&flet difference (earlier later)
                  (when (and earlier later)
                    (local-time:timestamp-difference later earlier))))
               ((&flet lookup (key)
                  (cons key (timestamp event key))))
               ((&flet maybe-timestamp< (left right)
                  (when (and left right)
                    (local-time:timestamp< left right))))
               (framework-cells (mapcar #'lookup *framework-timestamps*))
               (other-keys      (set-difference (timestamp-keys event)
                                                *framework-timestamps*))
               (other-cells     (sort (mapcar #'lookup other-keys)
                                      #'local-time:timestamp< :key #'cdr))
               (sorted          (merge 'list framework-cells other-cells
                                       #'maybe-timestamp< :key #'cdr))
               (names           (mapcar (compose #'timestamp-name #'car) sorted))
               (width           (length (extremum names #'> :key #'length)))
               (values          (mapcar #'cdr sorted))
               (differences     (mapcar #'difference (list* nil values) values)))
          (format stream "~:[~;~@:_~]Timestamps~
                          ~@:_~2@T~@<~
                            ~{~{~
                              ~VA ~:[~
                                ~*<none>~
                              ~:;~
                                ~:*~A~@[ (~:/rsb.formatting::print-human-readable-duration/)~]~
                              ~]~
                            ~}~^~@:_~}~
                          ~:>"
                  produced-output?
                  (mapcar #'list (circular-list width) names values differences)))
        (setf produced-output? t))

      ;; Meta-data.
      (when user-items?
        (when-let ((meta-data (remove-from-plist
                               (meta-data-plist event)
                               :rsb.transport.payload-size
                               :rsb.transport.wire-schema)))
          (format stream "~:[~;~@:_~]Meta-Data~
                          ~@:_~2@T~@<~
                            ~{~A ~S~^~@:_~}~
                          ~:>"
                  produced-output? meta-data)
          (setf produced-output? t)))

      ;; Causes
      (when causes?
        (when-let ((causes (event-causes event)))
          (format stream "~:[~;~@:_~]Causes~
                          ~@:_~2@T~@<~
                            ~{~A~^~@:_~}~
                          ~:>"
                  produced-output?
                  (map 'list #'event-id->uuid (event-causes event))))))

    (terpri stream)))

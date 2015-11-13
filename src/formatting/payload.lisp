;;;; payload.lisp --- Formatting methods for different kinds of event payloads.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.formatting)

(defmethod format-payload ((payload t) (style t) (stream t) &key)
  "Default behavior is to print PAYLOAD using the Lisp printer."
  (format stream "~@<~A~@:>" payload))

(defmethod format-payload ((payload vector) (style t) (stream t) &key)
  ;; Format PAYLOAD in form of a hexdump if it is an octet-vector.
  (if (typep payload 'octet-vector)
      (let+ ((length (length payload))
             ((&values &ign &ign end)
              (utilities.binary-dump:binary-dump
               payload
               :end         nil ; ignore *print-length*
               :lines       (when *print-lines*
                              (max 0 (1- *print-lines*)))
               :stream      stream
               :base        16
               :offset-base 16)))
        (unless (= end length)
          (format stream "~@:_[~:D octet~:P omitted]" (- length end))))
      (call-next-method)))

(defmethod format-payload ((payload string) (style t) (stream t) &key)
  "Format PAYLOAD as a multi-line string, trying to honor MAX-LINES
   and MAX-COLUMNS constraints."
  (format-string stream payload))

(defmethod format-payload ((payload scope) (style t) (stream t) &key)
  (write-string (scope-string payload) stream))

(defmethod format-payload ((payload standard-object) (style t) (stream t) &key)
  "Recursively format the slot values of PAYLOAD."
  (format-instance stream payload))

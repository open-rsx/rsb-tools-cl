;;;; payload.lisp --- Formatting methods for different kinds of event payloads.
;;;;
;;;; Copyright (C) 2011, 2013, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.formatting)

(defmethod format-payload :around ((payload t) (style t) (stream t)
                                   &key
                                   (max-lines   50)
                                   (max-columns 80)) ; TODO(jmoringe): default from clon?
  "Supply payload-independent defaults for MAX-LINES and MAX-COLUMNS."
  (let ((*print-lines*        max-lines)
        (*print-right-margin* max-columns)
        (*print-miser-width*  most-positive-fixnum))
    (call-next-method payload style stream
                      :max-lines   max-lines
                      :max-columns max-columns)))

(defmethod format-payload ((payload t) (style t) (stream t)
                           &key
                           max-lines
                           max-columns)
  "Default behavior is to print PAYLOAD using the Lisp printer."
  (let ((*print-miser-width*  (floor max-columns 2))
        (*print-right-margin* max-columns))
    (format stream "~A" payload)))

(defmethod format-payload ((payload vector) (style t) (stream t)
                           &key
                           max-lines
                           max-columns)
  ;; Format PAYLOAD in form of a hexdump if it is an octet-vector.
  (if (typep payload 'octet-vector)
      (let+ ((length (length payload))
             ((&values &ign &ign end)
              (utilities.binary-dump:binary-dump
               payload
               :end         nil         ; ignore *print-length*
               :lines       (1- max-lines)
               :stream      stream
               :width       max-columns
               :base        16
               :offset-base 16)))
        (unless (= end length)
          (format stream "~@:_[~:D octet~:P omitted]" (- length end))))
      (call-next-method)))

(defmethod format-payload ((payload string) (style t) (stream t)
                           &key
                           max-lines
                           max-columns)
  "Format PAYLOAD as a multi-line string, trying to honor MAX-LINES
   and MAX-COLUMNS constraints."
  (format-string stream payload))

(defmethod format-payload ((payload scope) (style t) (stream t)
                           &key
                           max-lines
                           max-columns)
  (declare (ignore max-lines max-columns))
  (write-string (scope-string payload) stream))

(defmethod format-payload ((payload standard-object) (style t) (stream t)
                           &key
                           max-lines
                           max-columns)
  "Recursively format the slot values of PAYLOAD."
  (format-instance stream payload))

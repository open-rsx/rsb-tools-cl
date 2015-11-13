;;;; payload.lisp --- Formatting methods for different kinds of event payloads.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.formatting)

;;; `payload-style-generic/pretty'

(defclass payload-style-generic/pretty ()
  ()
  (:documentation
   "Generic formatting for arbitrary payloads."))

(service-provider:register-provider/class
 'style :payload-generic/pretty :class 'payload-style-generic/pretty)

(defmethod format-payload ((payload t)
                           (style   payload-style-generic/pretty)
                           (stream  t)
                           &key)
  ;; Default behavior is to print PAYLOAD using the Lisp pretty
  ;; printer.
  (format stream "~@<~A~@:>" payload))

(defmethod format-payload ((payload vector)
                           (style   payload-style-generic/pretty)
                           (stream  t)
                           &key)
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

(defmethod format-payload ((payload string)
                           (style   payload-style-generic/pretty)
                           (stream  t)
                           &key)
  ;; Format PAYLOAD as a multi-line string, trying to honor
  ;; `*print-lines*' and `*print-length*' constraints.
  (format-string stream payload))

(defmethod format-payload ((payload scope)
                           (style   payload-style-generic/pretty)
                           (stream  t)
                           &key)
  (write-string (scope-string payload) stream))

(defmethod format-payload ((payload standard-object)
                           (style   payload-style-generic/pretty)
                           (stream  t)
                           &key)
  ;; Recursively format the slot values of PAYLOAD.
  (format-instance stream payload))

;;; `payload-style-generic/raw'

(defclass payload-style-generic/raw ()
  ()
  (:documentation
   "Raw formatting for binary payloads."))

(service-provider:register-provider/class
 'style :payload-generic/raw :class 'payload-style-generic/raw)

(defmethod format-payload ((payload simple-array)
                           (style   payload-style-generic/raw)
                           (stream  t)
                           &key)
  (write-sequence payload stream)
  (force-output stream))

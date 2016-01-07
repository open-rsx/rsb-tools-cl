;;;; util.lisp --- Utility functions used by the stats module.
;;;;
;;;; Copyright (C) 2012, 2013, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.stats)

;;; Extractor functions

(defun event-size (event &optional (replacement-value :n/a))
  "Try to determine and return the size of the payload of EVENT in
   bytes. Return REPLACEMENT-VALUE, if the size cannot be determined."
  (labels ((payload-size (payload)
             (typecase payload
               (integer
                (ceiling (integer-length payload) 8))
               (rsb.converter::annotated
                (payload-size (rsb.converter::annotated-wire-data payload)))
               ((cons t (not list))
                replacement-value)
               (sequence
                (length payload))
               (t
                replacement-value))))
    (or (meta-data event :rsb.transport.payload-size)
        (payload-size (event-data event)))))

(defun event-size/power-of-2 (event &optional (replacement-value :n/a))
  "Like `event-size', but the returned size is rounded to the nearest
   power of two, if it is a positive integer."
  (let ((size (event-size event replacement-value)))
    (if (typep size 'non-negative-integer)
        (ash 1 (integer-length size))
        size)))

(defun event-type/simple (event)
  "Return an object designating the type of EVENT."
  (cond
    ((meta-data event :rsb.transport.wire-schema))
    ((typep (event-data event) 'rsb.converter::annotated)
     (cons 'rsb.converter::annotated
           (rsb.converter::annotated-wire-schema (event-data event))))
    (t
     (type-of (event-data event)))))

;;; Printer

(defun print-quantity-value (stream quantity &optional colon? at?)
  "Print value of QUANTITY to STREAM.
   colon? and at? are ignored."
  (declare (ignore colon? at?))
  (format-value quantity stream))

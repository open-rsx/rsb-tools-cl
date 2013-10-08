;;;; util.lisp --- Extractor utility functions.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsb.stats)

(defun event-size (event &optional (replacement-value :n/a))
  "Try to determine and return the size of the payload of EVENT in
bytes. Return REPLACEMENT-VALUE, if the size cannot be determined."
  (or (meta-data event :rsb.transport.payload-size)
      (let ((data (event-data event)))
	(typecase data
	  (integer
	   (ceiling (integer-length data) 8))
	  (sequence
	   (length data))
	  (t
	   replacement-value)))))

(defun event-size/power-of-2 (event &optional (replacement-value :n/a))
  "Like `event-size', but the returned size is rounded to the nearest
power of two, if it is a positive integer."
  (let ((size (event-size event replacement-value)))
    (if (typep size 'non-negative-integer)
	(ash 1 (integer-length size))
	size)))

(defun event-type/simple (event)
  "Return an object designating the type of EVENT."
  (or (meta-data event :rsb.transport.wire-schema)
      (type-of (event-data event))))

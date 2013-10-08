;;;; temporal-bounds-mixin.lisp --- Lower and upper temporal bound.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsb.formatting)

(defclass temporal-bounds-mixin ()
  ((lower-bound :initarg  :lower-bound
		:type     time-spec
		:accessor lower-bound
		:initform '(- :now 20)
		:documentation
		"Stores a specification of the lower bound of the
temporal interval of interest. See type `time-spec'.")
   (upper-bound :initarg  :upper-bound
		:type     time-spec
		:accessor upper-bound
		:initform :now
		:documentation
		"Stores a specification of the upper bound of the
temporal interval of interest. See type `time-spec'."))
  (:documentation
   "This mixin adds lower and upper temporal bounds which can be
provided in the form of abstract specifications. Realizations of these
specifications can be retrieved for concrete points in time."))

(defmethod bounds ((thing temporal-bounds-mixin))
  (list (lower-bound thing) (upper-bound thing)))

(defmethod (setf bounds) ((new-value list)
			  (thing     temporal-bounds-mixin))
  (check-type new-value bounds-spec)

  (setf (lower-bound thing) (first  new-value)
	(upper-bound thing) (second new-value)))

(defmethod bounds/expanded ((thing temporal-bounds-mixin))
  (let+ ((now)
	 ((&flet now ()
	    (or now
		(setf now (timestamp->unix/nsecs (local-time:now)))))))
    (values (list (%expand-time-spec (lower-bound thing) #'now)
		  (%expand-time-spec (upper-bound thing) #'now))
	    (now))))

(defmethod range/expanded ((thing temporal-bounds-mixin))
  (let+ (((lower upper) (bounds/expanded thing)))
    (- upper lower)))


;;; Utility functions
;;

(defun %expand-time-spec (spec now)
  "Translate SPEC into an absolute timestamp of type
`timestamp/unix/nsec' and return the timestamp.

If the translation requires the current time, NOW is called without
arguments to retrieve it."
  (etypecase spec
    ((eql :now)
     (funcall now))

    ((cons (member + - * /))
     (apply (first spec)
	    (mapcar (rcurry #'%expand-time-spec now) (rest spec))))

    (real
     (floor spec 1/1000000000))))

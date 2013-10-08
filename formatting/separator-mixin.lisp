;;;; separator-mixin.lisp --- Mixin for printing separators.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsb.formatting)

(defclass separator-mixin ()
  ((separator :type     separator-spec
	      :accessor style-separator
	      :initform #\Newline
	      :documentation
	      "The character or pattern by means of which items should
be separated in the output."))
  (:documentation
   "This class is intended to be mixed into style classes that should
print separators between output items."))

(defmethod shared-initialize :after ((instance   separator-mixin)
                                     (slot-names t)
                                     &key
				     (separator nil separator-supplied?))
  (when separator-supplied?
    (setf (style-separator instance) separator)))

(defmethod (setf style-separator) :before ((new-value t)
					   (style     separator-mixin))
  (check-type new-value separator-spec "a valid separator specification"))

(defmethod format-event :before ((event  t)
				 (style  separator-mixin)
				 (stream t)
				 &key
				 (max-columns (or *print-right-margin* 80))
				 &allow-other-keys)
  "Print a separator before each event."
  (print-separator (style-separator style) stream max-columns))


;;; Utility functions
;;

(defun print-separator (spec stream max-columns)
  "Print a separator according to SPEC onto STREAM."
  (etypecase spec
    (null)

    ((or character string)
     (princ spec stream))

    (rule-spec
     (princ (make-string max-columns :initial-element (second spec))
	    stream))

    ((eql :clear)
     (format stream "~C~A~C~A" #\Escape "[1;1f" #\Escape "[J"))

    (list
     (map nil (rcurry #'print-separator stream max-columns) spec))))

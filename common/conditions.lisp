;;;; conditions.lisp --- Conditions used in the cl-rsb-common system.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsb.common)

(define-condition failed-to-load-idl (rsb-error
				      chainable-condition)
  ((source :initarg  :source
	   :reader   failed-to-load-idl-source
	   :documentation
	   ""))
  (:report
   (lambda (condition stream)
     (format stream "~@<Failed to load data definition from source ~
~A.~/more-conditions::maybe-print-cause/~@:>"
	     (failed-to-load-idl-source condition)
	     condition)))
  (:documentation
   "This error is signaled when an attempt to load a data definition
from some source fails."))

(defun failed-to-load-idl (source &optional cause)
  "Convenience function for signaling `failed-to-load-idl'."
  (apply #'error 'failed-to-load-idl
	 :source source
	 (when cause
	   (list :cause cause))))

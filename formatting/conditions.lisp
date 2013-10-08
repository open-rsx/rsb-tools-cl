;;;; conditions.lisp --- Conditions used in the formatting module.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.formatting)

(define-condition format-code-error (error)
  ((code :initarg  :code
         :reader   format-code-error-code
         :documentation
         "Stores the user-supplied format code that caused the
problem."))
  (:report
   (lambda (condition stream)
     (format stream "~@<Failed to use format code ~S.~@:>"
             (format-code-error-code condition))))
  (:documentation
   "This error is signaled when an error related to user-supplied
format code occurs."))

(define-condition simple-format-code-error (format-code-error
                                            simple-error)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<Failed to use format code ~S~@:>"
             (format-code-error-code condition))
     (maybe-print-explanation stream condition)))
  (:documentation
   "This `simple-error' is signaled when an error related to
user-supplied format code occurs."))

(defun format-code-error (code format-control &rest format-arguments)
  "Signal a `simple-format-code-error' with description FORMAT-CONTROL
and FORMAT-ARGUMENTS when user-supplied CODE caused a problem."
  (error 'simple-format-code-error
         :code             code
         :format-control   format-control
         :format-arguments format-arguments))

(define-condition format-code-read-error (format-code-error
                                          chainable-condition)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<Failed to read formatting code from ~S~@:>."
             (format-code-error-code condition))
     (maybe-print-cause stream condition)))
  (:documentation
   "This error is signaled when reading user-supplied format code
fails."))

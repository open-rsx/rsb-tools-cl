;;;; conditions.lisp --- Conditions used in the rsb-tools-common system.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.tools.common)

;;; Event and payload-related conditions

(define-condition call-specification-error (rsb-error
                                            simple-error
                                            chainable-condition)
  ((specification :initarg  :specification
                  :type     string
                  :reader   call-specification-error-specification
                  :documentation
                  "Stores the invalid specification."))
  (:default-initargs
   :specification (missing-required-initarg 'call-specification-error :specification))
  (:report
   (lambda (condition stream)
     (format stream "~@<Error parsing call specification ~S~
                     ~/more-conditions:maybe-print-explanation/~
                     ~:*~/more-conditions:maybe-print-cause/~@:>"
             (call-specification-error-specification condition)
             condition)))
  (:documentation
   "This error is signaled when a call specification cannot be
    parsed."))

;;; IDL-related conditions

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

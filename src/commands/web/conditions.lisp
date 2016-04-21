;;;; conditions.lisp --- Conditions used in the commands.web module.
;;;;
;;;; Copyright (C) 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.tools.commands.web)

(define-condition argument-condition (condition)
  ((parameter :initarg  :parameter
              :reader   argument-condition-parameter
              :documentation
              "Stores the name of request parameter that is the
               subject of the condition."))
  (:default-initargs
   :parameter (missing-required-initarg 'argument-condition :parameter))
  (:documentation
   "This condition class adds a slot storing the name of request
    parameter that is the subject of the condition."))

(define-condition argument-error (error
                                  argument-condition)
  ()
  (:documentation
   "Subclasses of this condition class indicate problems with request
    arguments."))

(define-condition simple-argument-error (argument-error
                                         simple-error
                                         chainable-condition)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<~?~/more-conditions:maybe-print-cause/~@:>"
             (simple-condition-format-control   condition)
             (simple-condition-format-arguments condition)
             condition))))

(defun argument-error (parameter format-control &rest format-arguments)
  (error 'simple-argument-error
         :parameter        parameter
         :format-control   format-control
         :format-arguments format-arguments))

(define-condition argument-type-error (argument-error
                                        type-error)
  ()
  (:documentation
   "This error is signaled when a request argument is not of the
    expected type."))

(define-condition argument-parse-error (argument-error
                                        chainable-condition)
  ((raw-value :initarg :raw-value
              :reader  argument-parse-error-raw-value
              :documentation
              "Stores the unparsed value of the offending request
               argument."))
  (:report
   (lambda (condition stream)
     (format stream "~@<Could not parse value ~S of parameter ~
                     ~S~/more-conditions:maybe-print-cause/~@:>"
             (argument-parse-error-raw-value condition)
             (argument-condition-parameter   condition)
             condition)))
  (:documentation
   "This error is signaled when a request argument cannot be
    parsed."))

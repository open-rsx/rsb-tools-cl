;;;; conditions.lisp --- Conditions used by the stats module.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.stats)

(define-condition quantity-creation-error (error
                                           chainable-condition)
  ((specification :initarg  :specification
                  :reader   quantity-creation-error-specification
                  :documentation
                  "Stores the specification according to which the
                   quantity instance should have been constructed."))
  (:report
   (lambda (condition stream)
     (format stream "~@<Failed to create quantity according to ~
                     specification~
                     ~@:_~@:_~
                     ~2@T~<~{~S~^ ~}~:>~
                     ~@:_~@:_~
                     .~/more-conditions:maybe-print-cause/~@:>"
             (list (quantity-creation-error-specification condition))
             condition)))
  (:documentation
   "This error is signaled when attempt to construct a quantity
    instance fails."))

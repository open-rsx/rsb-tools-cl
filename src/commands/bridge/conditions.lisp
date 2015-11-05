;;;; conditions.lisp --- Conditions used in the commands.bridge module.
;;;;
;;;; Copyright (C) 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.tools.commands.bridge)

;;; Specification conditions

(define-condition specification-condition (rsb-problem-condition
                                           chainable-condition)
  ((spec :initarg  :spec
         :reader   specification-condition-spec
         :documentation
         "The problematic specification."))
  (:default-initargs
   :spec (missing-required-initarg 'specification-condition :spec))
  (:documentation
   "Superclass for condition classes indicating problems with bridge
    specifications."))

(define-condition specification-error (specification-condition
                                       rsb-error)
  ()
  (:report
   (lambda (condition stream)
     (let ((*print-circle* t))
       (format stream "~@<Bridge specification ~A is ~
                       invalid.~/more-conditions:maybe-print-cause/~@:>"
               (specification-condition-spec condition)
               condition))))
  (:documentation
   "This error is signaled when a bridge specification is invalid."))

;;; Forwarding cycle conditions

(define-condition forwarding-cycle-condition (rsb-problem-condition)
  ((source      :initarg  :source
                :reader   forwarding-cycle-condition-source
                :documentation
                "The event source involved in the forwarding cycle.")
   (destination :initarg :destination
                :reader   forwarding-cycle-condition-destination
                :documentation
                "The event sink involved in the forwarding cycle."))
  (:default-initargs
   :source      (missing-required-initarg 'forwarding-cycle-condition :source)
   :destination (missing-required-initarg 'forwarding-cycle-condition :destination))
  (:documentation
   "Superclass for condition classes indicating forwarding cycles in a
    bridge configuration."))

(define-condition forwarding-cycle-warning (forwarding-cycle-condition
                                            rsb-warning)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<Events published by ~A could potentially be ~
                     received by ~A, creating a forwarding cycle.~@:>"
             (forwarding-cycle-condition-destination condition)
             (forwarding-cycle-condition-source      condition))))
  (:documentation
   "This warning is signaled when it has been determined that a bridge
    configuration could potentially create a forwarding cycle.

    See the documentation of the `rsb.model.inference' package for the
    meaning of \"potentially\" in this context."))

(define-condition forwarding-cycle-error (forwarding-cycle-condition
                                          rsb-error)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<Events published by ~A would be received by ~
                     ~A, creating a forwarding cycle.~@:>"
             (forwarding-cycle-condition-destination condition)
             (forwarding-cycle-condition-source      condition))))
  (:documentation
   "This error is signaled when it has been determined that a bridge
    configuration would definitely create a forwarding cycle.

    See the documentation of the `rsb.model.inference' package for the
    meaning of \"definitely\" in this context."))

;;;; protocol.lisp --- Protocol functions of the stats module.
;;;;
;;;; Copyright (C) 2011, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.stats)

;;; Quantity protocol

(defgeneric quantity-name (quantity)
  (:documentation
   "Return the name of QUANTITY."))

(defgeneric quantity-value (quantity)
  (:documentation
   "Return the value of QUANTITY."))

(defgeneric update! (quantity event)
  (:documentation
   "Update the state of QUANTITY using data from EVENT. Most
    quantities extract some part of EVENT, possible derive some datum
    through a transformation and add the datum to a collection from
    which the actual value of the quantity is computed."))

(defgeneric reset! (quantity)
  (:documentation
   "Reset the state of QUANTITY. For quantities that accumulate values
    like moments-based quantities or histogram-based quantities this
    clear the collection of accumulated values."))

(defgeneric format-value (quantity stream)
  (:documentation
   "Format the value of QUANTITY onto STREAM."))

;;; Collecting quantity protocol

(defgeneric quantity-values (quantity)
  (:documentation
   "Return the values that have been collected in order to compute the
    value of quantity. Only applicable to quantities that accumulate
    values in some way."))

;;; Quantity service and creation

(service-provider:define-service quantity
  (:documentation
   "Providers of this service are classes implementing the quantity
    protocol."))

(defgeneric make-quantity (spec &rest args)
  (:documentation
   "Make and return a quantity instance according to SPEC. SPEC can
    either be a keyword, designating a quantity class, a list of the
    form

      (CLASS KEY1 VALUE1 KEY2 VALUE2 ...)

    designating a quantity class and specifying initargs, or a
    quantity instance."))

(define-condition-translating-method make-quantity (spec &rest args)
  ((error quantity-creation-error)
   :specification (append (ensure-list spec) args)))

(defmethod make-quantity ((spec symbol) &rest args)
  (apply #'service-provider:make-provider 'quantity spec args))

(defmethod make-quantity ((spec cons) &rest args)
  (check-type spec (cons keyword list) "a keyword followed by initargs")
  (if args
      (apply #'make-quantity (first spec) (append args (rest spec)))
      (apply #'make-quantity spec)))

(defmethod make-quantity ((spec standard-object) &rest args)
  (when args
    (apply #'incompatible-arguments :spec spec args))
  spec)

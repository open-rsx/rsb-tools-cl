;;;; protocol.lisp --- Protocol functions of the stats module.
;;;;
;;;; Copyright (C) 2011, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.stats)

;;; Quantity protocol

(defgeneric quantity-name (quantitiy)
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

;;; Quantity class family

(dynamic-classes:define-findable-class-family quantity
  "This class family consists of quantity classes.")

(defun make-quantity (spec)
  "Make and return a quantity instance according to SPEC. SPEC can
   either be a keyword, designating a quantity class, a list of the
   form

     (CLASS KEY1 VALUE1 KEY2 VALUE2 ...)

   designating a quantity class and specifying initargs, or a quantity
   instance."
  (etypecase spec
    (keyword
     (make-instance (rsb.stats:find-quantity-class spec)))
    (list
     (check-type spec (cons keyword list) "a keyword followed by initargs")
     (let+ (((class &rest args) spec))
       (apply #'make-instance (rsb.stats:find-quantity-class class) args)))
    (standard-object
     spec)))

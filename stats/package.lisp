;;;; package.lisp --- Package definition for stats module.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage :rsb.stats
  (:use
   :cl
   :alexandria
   :let-plus
   :more-conditions

   :rsb)

  ;; Types
  (:export
   :meta-data-selector
   :when-missing-policy)

  ;; Quantity protocol
  (:export
   :quantity-name
   :quantity-value

   :update!
   :reset!

   :format-value)

  ;; `collecting-mixin' mixin class
  (:export
   :quantity-values

   :collecting-mixin)

  ;; `extract-function-mixin' mixin class
  (:export
   :quantity-extractor

   :extract-function-mixin)

  ;; `histogram-mixin' mixin class
  (:export
   :histogram-mixin)

  ;; `moments-mixin' mixin class
  (:export
   :moments-mixin)

  ;; `all-time-mixin' mixin class
  (:export
   :all-time-mixin)

  ;; `named-mixin'
  (:export
   :named-mixin)

  ;; `reduction-mixin' mixin class
  (:export
   :quantity-reduce-by

   :reduction-mixin)

  ;; `rate-mixin' mixin class
  (:export
   :rate-mixin)

  ;; `meta-data-mixin' mixin class
  (:export
   :meta-data-mixin

   :quantity-key
   :quantity-when-missing)

  ;; `format-mixin' mixin class
  (:export
   :format-mixin

   :quantity-format)

  ;; Quantity findable class family
  (:export
   :no-such-quantity-class
   :find-quantity-class
   :quantity-classes)

  ;; Extractor utilities
  (:export
   :event-size
   :event-size/power-of-2

   :event-type/simple)

  (:documentation
   "This package contains functions and classes for computing basic
statistical quantities over properties of RSB events. Most quantities
collect some event property over a period of time and produce some
aggregated value from the collection.

Examples include
+ Event frequency
+ Histogram of event origins
+ Histogram of event wire-schema
+ Mean and variance of event payload size
+ Mean and variance event latency"))

;;; package.lisp --- Package definition for stats module.
;;
;; Copyright (C) 2011 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This Program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This Program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses>.

(cl:defpackage :rsb.stats
  (:use
   :cl
   :alexandria
   :bind

   :rsb)

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

  ;; Quantity findable class family
  (:export
   :no-such-quantity-class
   :find-quantity-class
   :quantity-classes)

  (:documentation
   "This package contains functions and classes for computing basic
statistical quantities over properties of RSB events. Most quantities
collect some event property over a period of time and produce some
aggregated value from the collection. Examples include
+ Event frequency
+ Histogram of event origins
+ Histogram of event wire-schema
+ Mean and variance of event payload size
+ Mean and variance event latency"))
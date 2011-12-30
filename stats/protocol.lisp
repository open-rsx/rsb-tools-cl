;;; protocol.lisp --- Protocol functions of the stats module.
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

(cl:in-package :rsb.stats)


;;; Quantity protocol
;;

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
through a transformation and add the datum to a collection from which
the actual value of the quantity is computed."))

(defgeneric reset! (quantity)
  (:documentation
   "Reset the state of QUANTITY. For quantities that accumulate values
like moments-based quantities or histogram-based quantities this clear
the collection of accumulated values."))

(defgeneric format-value (quantity stream)
  (:documentation
   "Format the value of QUANTITY onto STREAM."))


;;; Collecting quantity protocol
;;

(defgeneric quantity-values (quantity)
  (:documentation
   "Return the values that have been collected in order to compute the
value of quantity. Only applicable to quantities that accumulate
values in some way."))


;;; Quantity class family
;;

(dynamic-classes:define-findable-class-family quantity
    "This class family consists of quantity classes.")

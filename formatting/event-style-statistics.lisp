;;; event-style-statistics.lisp --- Column-oriented formatting of statistics.
;;
;; Copyright (C) 2011, 2012 Jan Moringen
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

(cl:in-package :rsb.formatting)


;;; Class `statistics-columns-mixin'
;;

(defclass statistics-columns-mixin (columns-mixin)
  ((quantities :type     list
	       :accessor %style-quantities
	       :initform nil
	       :documentation
	       "Stores the list of quantities printed by the
formatting style."))
  (:documentation
   "This class is intended to be mixed into formatting classes that
present the values of statistical quantities in a column-based
manner."))

(defmethod (setf style-columns) :after ((new-value t)
					(style     statistics-columns-mixin))
  ;; Find columns in NEW-VALUE that contain a quantity. Extract and
  ;; store these quantities for direct updates.
  (let+ (((&flet quantity-column? (column)
	    (closer-mop:compute-applicable-methods-using-classes
	     (fdefinition 'column-quantity)
	     (list (class-of column))))))
    (setf (%style-quantities style)
	  (map 'list #'column-quantity
	       (remove-if-not #'quantity-column? (style-columns style))))))

(defmethod format-event :around ((event  t)
				 (style  statistics-columns-mixin)
				 (stream t)
				 &key &allow-other-keys)
  ;; Update quantities.
  (if (eq event :trigger)
      (call-next-method)
      (map nil (rcurry #'rsb.stats:update! event)
	   (%style-quantities style))))


;;; Class `style-statistics'
;;

(defmethod find-style-class ((spec (eql :statistics)))
  (find-class 'style-statistics))

(defclass style-statistics (periodic-printing-mixin
			    statistics-columns-mixin
			    header-printing-mixin)
  ((quantities :type     list
	       :accessor %style-quantities
	       :initform nil
	       :documentation
	       "Stores the list of quantities printed by the
formatting style."))
  (:default-initargs
   :columns '(:now
	      (:quantity :quantity :rate       :width 12)
	      (:quantity :quantity :throughput :width 16)
	      (:quantity :quantity (:latency
				    :from :create
				    :to   :deliver
				    :name "Latency"))
	      :newline))
  (:documentation
   "This formatting style computes a number of configurable
statistical quantities from received events collected over a
configurable period of time and prints the computed values in a
tabular manner."))

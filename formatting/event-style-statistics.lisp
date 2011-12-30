;;; event-style-statistics.lisp --- Column-oriented formatting of statistics.
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

(cl:in-package :rsb.formatting)

(defmethod find-style-class ((spec (eql :statistics)))
  (find-class 'style-statistics))

(defclass style-statistics (periodic-printing-mixin
			    header-printing-mixin
			    columns-mixin)
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

(defmethod (setf style-columns) :after ((new-value t)
					(style     style-statistics))
  ;; Find columns in NEW-VALUE that contain a quantity. Extract and
  ;; store these quantities for direct updates.
  (bind (((:flet quantity-column? (column))
	  (closer-mop:compute-applicable-methods-using-classes
	   (fdefinition 'column-quantity)
	   (list (class-of column)))))
    (setf (%style-quantities style)
	  (map 'list #'column-quantity
	       (remove-if-not #'quantity-column? (style-columns style))))))

(defmethod format-event :around ((event  event)
				 (style  style-statistics)
				 (stream t)
				 &key &allow-other-keys)
  ;; Updated quantities.
  (map nil (rcurry #'rsb.stats:update! event) (%style-quantities style))

  ;; Give `periodic-printing-mixin' a chance to grab STREAM.
  (call-next-method))

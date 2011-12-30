;;; collecting-mixin.lisp --- A mixin class for collection of values.
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

(defclass collecting-mixin ()
  ((values :initarg  :values
	   :type     vector
	   :reader   quantity-values
	   :initform (make-array 0
				 :fill-pointer 0
				 :adjustable   t)
	   :documentation
	   "Stores the values that have been collected from events."))
  (:documentation
   "This mixin is intended to be added to quantity classes the values
of which are computed by accumulating auxiliary values across multiple
events."))

(defmethod update! ((quantity collecting-mixin)
		    (value    (eql nil)))
  "Ignore nil value by default."
  (values))

(defmethod update! ((quantity collecting-mixin)
		    (value    t))
  "Add VALUE to the collected values."
  (vector-push-extend value (quantity-values quantity))
  (when (next-method-p)
    (call-next-method)))

(defmethod reset! ((quantity collecting-mixin))
  "Clear the collection of values in QUANTITY."
  (setf (fill-pointer (quantity-values quantity)) 0)
  (when (next-method-p)
    (call-next-method)))

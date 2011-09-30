;;; quantity-column.lisp ---
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

(in-package :rsb.formatting)

(defmethod find-column-class ((spec (eql :quantity)))
  (find-class 'quantity-column))

(defclass quantity-column (width-mixin)
  ((quantity :accessor column-quantity
	     :documentation
	     "Stores the underlying quantity instances printed by this
column."))
  (:default-initargs
   :quantity (missing-required-initarg 'quantity-column :quantity))
  (:documentation
   "Instances of this class use an associated quantity instance to
provide a name and the printed value."))

(defmethod shared-initialize :after ((instance   quantity-column)
                                     (slot-names t)
                                     &key
				     quantity)
  (when quantity
    (setf (column-quantity instance) (make-quantity quantity))))

(defmethod column-name ((column quantity-column))
  (rsb.stats:quantity-name (column-quantity column)))

(defmethod format-event ((event  (eql :trigger))
			 (style  quantity-column)
			 (stream t)
			 &key &allow-other-keys)
  (bind (((:accessors-r/o (quantity column-quantity)) style))
    (rsb.stats:format-value quantity stream)
    (rsb.stats:reset! quantity)))

(defmethod print-object ((object quantity-column) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A = " (column-name object))
    (rsb.stats:format-value (column-quantity object) stream)))


;;; Utility functions
;;

(defun make-quantity (spec)
  "Make and return a quantity instance according to SPEC. SPEC can
either be a keyword, designating a quantity class, a list of the form

  (CLASS KEY1 VALUE1 KEY2 VALUE2 ...)

designating a quantity class and specifying initargs, or a quantity
instance."
  (etypecase spec
    (keyword
     (make-instance (rsb.stats:find-quantity-class spec)))
    (list
     (check-type spec (cons keyword list) "a keyword followed by initargs")
     (bind (((class &rest args) spec))
       (apply #'make-instance (rsb.stats:find-quantity-class class) args)))
    (standard-object
     spec)))

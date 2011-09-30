;;; reduction-mixin.lisp --- A mixin class for reduction-based quantities.
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

(in-package :rsb.stats)

(defclass reduction-mixin ()
  ((reduce-by :initarg  :reduce-by
	      :type     function
	      :accessor quantity-reduce-by
	      :documentation
	      "Stores the reduce function that produces the value of
the quantity by reducing a collection of values."))
  (:documentation
   "This mixin class is intended to be mixed into quantity classes
which compute the quantity value from a collection of values using a
reduction function."))

(defmethod quantity-value ((quantity reduction-mixin))
  (bind (((:accessors-r/o (values    quantity-values)
			  (reduce-by quantity-reduce-by)) quantity))
    (if (emptyp values)
	:n/a
	(reduce reduce-by values))))

(defmethod format-value ((quantity reduction-mixin) (stream t))
  (format stream "~,3F" (quantity-value quantity)))

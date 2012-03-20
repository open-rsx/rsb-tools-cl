;;; sub-style-sorting-mixin.lisp --- Mixin for sorting sub-styles.
;;
;; Copyright (C) 2012 Jan Moringen
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

(defclass sub-style-sorting-mixin (delegating-mixin)
  ((sort-predicate :initarg  :sort-predicate
		   :type     function
		   :accessor style-sort-predicate
		   :documentation
		   "Stores the predicate according to which sub-styles
should be sorted when retrieved as a sorted list.")
   (sort-key       :initarg  :sort-key
		   :type     function
		   :accessor style-sort-key
		   :documentation
		   "Stores the key reader that should be used when
sorting sub-styles."))
  (:default-initargs
   :sort-predicate (missing-required-initarg
		    'sub-style-sorting-mixin :sort-predicate)
   :sort-key       (missing-required-initarg
		    'sub-style-sorting-mixin :sort-key))
  (:documentation
   "This mixin adds to delegating formatting style classes the ability
to retrieve sub-styles sorted according to a predicate."))

(defmethod style-sub-styles/sorted ((style sub-style-sorting-mixin)
				    &key
				    (predicate (style-sort-predicate style))
				    (key       (style-sort-key style)))
  (sort (map 'list #'cdr (style-sub-styles style))
	predicate
	:key key))

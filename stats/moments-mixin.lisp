;;; moments-mixin.lisp --- A mixin class for computing moments.
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

(cl:in-package :rsb.stats)

(defclass moments-mixin ()
  ()
  (:documentation
   "This mixin class is intended to be mixed into quantity classes
that provided mean and variance of a collection of accumulated
values."))

(defmethod quantity-value ((quantity moments-mixin))
  (let+ (((&accessors-r/o (values quantity-values)) quantity))
    (if (emptyp values)
	(values :n/a          :n/a)
	(values (mean values) (standard-deviation values)))))

(defmethod format-value ((quantity moments-mixin) (stream t))
  (apply #'format stream "~,3F ± ~,3F"
	 (multiple-value-list (quantity-value quantity))))

;;; all-time-mixin.lisp --- Continuous aggregation without reset.
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

(cl:in-package :rsb.stats)

(defclass all-time-mixin (collecting-mixin
			  reduction-mixin)
  ()
  (:documentation
   "This class is intended to be mixed into quantity classes which
continuously aggregate new values into the quantity value. Examples
include size of all transferred data in a whole session."))

(defmethod reset! ((quantity all-time-mixin))
  (setf (quantity-empty-value quantity) (quantity-value quantity))
  (when (next-method-p)
    (call-next-method)))

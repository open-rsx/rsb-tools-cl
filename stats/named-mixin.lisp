;;; named-mixin.lisp --- A mixin class for named quantities.
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

(defclass named-mixin ()
  ((name :initarg  :name
	 :type     string
	 :accessor quantity-name
	 :initform (missing-required-initarg 'named-mixin :name)
	 :documentation
	 "Stores the name of the quantity."))
  (:documentation
   "This mixin class is intended to be mixed into quantity classes to
take care of the quantity name."))

(defmethod print-object ((object named-mixin) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A = " (quantity-name object))
    (format-value object stream)))

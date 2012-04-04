;;; format-mixin.lisp --- Configurable formatting of quantity values.
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

(defclass format-mixin ()
  ((format :initarg  :format
	   :type     string
	   :accessor quantity-format
	   :initform "~A"
	   :documentation
	   "Stores the format string that should be used to format the
value of the quantity."))
  (:documentation
   ""))

(defmethod format-value ((quantity format-mixin) (stream t))
  (format stream (quantity-format quantity) (quantity-value quantity)))

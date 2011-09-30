;;; counting-mixin.lisp --- Output counting style mixin class.
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

(defclass counting-mixin ()
  ((count :initarg  :count
	  :type     non-negative-integer
	  :accessor style-count
	  :initform 0
	  :documentation
	  "Stores the number of output cycles already performed by the
formatter."))
  (:documentation
   "This class is intended to be mixed into formatter classes that
need keep track of the number of performed output cycles."))

(defmethod format-event :after ((event  t)
				(style  counting-mixin)
				(stream t)
				&key &allow-other-keys)
  (incf (style-count style)))

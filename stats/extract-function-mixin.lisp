;;; extract-function-mixin.lisp --- A mixin class for flexible value extraction.
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

(defclass extract-function-mixin ()
  ((extractor :initarg  :extractor
	      :type     function
	      :accessor quantity-extractor
	      :initform (missing-required-initarg
			 'extract-function-mixin :extractor)
	      :documentation
	      "Stores a function that is called to extract a value
from some object."))
  (:documentation
   "This mixin class is intended to be mixed into quantity classes
that should provide flexible extraction of values from events or other
sources."))

(defmethod update! ((quantity extract-function-mixin)
		    (event    event))
  (update! quantity (funcall (quantity-extractor quantity) event)))

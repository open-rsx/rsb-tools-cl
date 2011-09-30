;;; histogram-mixin.lisp --- A mixin class for computing a histogram.
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

(defclass histogram-mixin ()
  ((values :initarg  :values
	   :type     hash-table
	   :reader   %quantity-values
	   :initform (make-hash-table :test #'equalp)
	   :documentation
	   "Stores a mapping from values in the quantity's domain to
the respective frequencies of these values."))
  (:documentation
   "This mixin class is intended to be mixed into quantity classes
that accumulate values in form of a histogram."))

(defmethod quantity-values ((quantity histogram-mixin))
  (hash-table-values (%quantity-values quantity)))

(defmethod quantity-value ((quantity histogram-mixin))
  (hash-table-alist (%quantity-values quantity)))

(defmethod update! ((quantity histogram-mixin)
		    (value    t))
  (incf (gethash value (%quantity-values quantity) 0)))

(defmethod reset! ((quantity histogram-mixin))
  (clrhash (%quantity-values quantity)))

(defmethod format-value ((quantity histogram-mixin)
			 (stream   t))
  (format stream "~:[N/A~;~:*~{~{~A: ~D~}~^, ~}~]"
	  (map 'list #'(lambda (cons) (list (car cons) (cdr cons)))
	       (sort (quantity-value quantity) #'> :key #'cdr))))

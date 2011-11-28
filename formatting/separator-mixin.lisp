;;; separator-mixin.lisp --- Mixin for printing separator rulers.
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

(defclass separator-mixin ()
  ((separator :type     separator-spec
	      :accessor style-separator
	      :initform #\Newline
	      :documentation
	      "The character or pattern by means of which items should
be separated in the output."))
  (:documentation
   "This class is intended to be mixed into style classes that should
print separators between output items."))

(defmethod shared-initialize :after ((instance   separator-mixin)
                                     (slot-names t)
                                     &key
				     (separator nil separator-supplied?))
  (when separator-supplied?
    (setf (style-separator instance) separator)))

(defmethod (setf style-separator) :before ((new-value t)
					   (style     separator-mixin))
  (check-type new-value separator-spec "a valid separator specification"))

(defmethod format-event :after ((event  event)
				(style  separator-mixin)
				(stream t)
				&key
				(max-columns (or *print-right-margin* 80))
				&allow-other-keys)
  "Print a separator after each event."
  (print-separator (style-separator style) stream max-columns))


;;; Utility functions
;;

(defun print-separator (spec stream max-columns)
  "Print a separator according to SPEC onto STREAM."
  (etypecase spec
    (null)
    ((or character string)
     (princ spec stream))
    (rule-spec
     (princ (make-string max-columns :initial-element (second spec))
	    stream))
    (list
     (map nil (rcurry #'print-separator stream max-columns) spec))))

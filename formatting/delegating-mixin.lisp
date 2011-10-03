;;; delegating-mixin.lisp --- Predicate-based delegation to sub-styles.
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

(defclass delegating-mixin ()
  ((sub-styles :type     list
	       :accessor style-sub-styles
	       :initform nil
	       :documentation
	       "Stores predicates and corresponding sub-styles as an
alist of items of the form (PREDICATE . SUB-STYLE)."))
  (:default-initargs
   :sub-styles (list (cons (constantly t) :self)))
  (:documentation
   "This class is intended to be used in formatting classes that
delegate to sub-styles based on dispatch predicates."))

(defmethod shared-initialize :after ((instance   delegating-mixin)
                                     (slot-names t)
                                     &key
				     sub-styles)
  (setf (style-sub-styles instance) sub-styles))

(defmethod (setf style-sub-styles) :around ((new-value sequence)
					    (object    delegating-mixin))
  (call-next-method (subst object :self new-value) object))

(defmethod format-event ((event  t)
			 (style  delegating-mixin)
			 (stream t)
			 &key &allow-other-keys)
  "Format EVENT on STREAM on a single line."
  (bind (((:accessors-r/o (sub-styles style-sub-styles)) style)
	 (sub-style (cdr (find-if (rcurry #'funcall event) sub-styles
				  :key #'car))))
    (cond
      ((null sub-style))
      ((eq sub-style style)
       (call-next-method))
      (t
       (format-event event sub-style stream)))))

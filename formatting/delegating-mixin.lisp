;;; delegating-mixin.lisp --- Predicate-based delegation to sub-styles.
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

(cl:in-package :rsb.formatting)

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

(defmethod sub-style-for ((style delegating-mixin)
			  (event t))
  "Return a list of sub-styles of STYLES whose predicate succeeds on
EVENT."
  (map 'list #'cdr
       (remove-if (complement (rcurry #'funcall event))
		  (style-sub-styles style)
		  :key #'car)))

(defmethod format-event ((event  t)
			 (style  delegating-mixin)
			 (stream t)
			 &key &allow-other-keys)
  "Delegate formatting of EVENT on STREAM to appropriate sub-styles of
STYLE."
  (let+ ((sub-styles (sub-style-for style event))
	 ((&labels apply-style (style-or-styles)
	    (cond
	      ((typep style-or-styles 'sequence)
	       (map nil #'apply-style style-or-styles))

	      ((eq style-or-styles style)
	       (call-next-method))

	      (t
	       (format-event event style-or-styles stream))))))
    (map nil #'apply-style sub-styles)))

(defmethod print-object ((object delegating-mixin) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "(~D)" (length (style-sub-styles object)))))

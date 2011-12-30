;;; meta-data-mixin.lisp --- Mixin class for meta-data-based quantities.
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

(defclass meta-data-mixin (named-mixin)
  ((key          :initarg  :key
		 :type     meta-data-selector
		 :accessor quantity-key
		 :documentation
		 "Stores the key for which meta-data items should be extracted
from processed events.")
   (when-missing :initarg  :when-missing
		 :type     when-missing-policy
		 :accessor quantity-when-missing
		 :initform :skip
		 :documentation
		 "Stores a designator the policy that should be
employed when a meta-data item is not available."))
  (:default-initargs
   :key (missing-required-initarg 'meta-data-mixin :key))
  (:documentation
   "This class is intended to be mixed into quantity classes that
process meta-data items of events."))

(defmethod shared-initialize :around ((instance   meta-data-mixin)
				      (slot-names t)
				      &rest args
				      &key
				      key
				      (name (when key (string key))))
  (check-type key meta-data-selector)

  (apply #'call-next-method instance slot-names
	 (append (when name (list :name name))
		 (remove-from-plist args :name))))

(defmethod update! ((quantity meta-data-mixin)
		    (event    event))
  (bind (((:accessors-r/o (key          quantity-key)
			  (when-missing quantity-when-missing)) quantity))
    (case key
      (:keys
       (map nil (curry #'update! quantity) (meta-data-keys event)))
      (:values
       (map nil (curry #'update! quantity) (meta-data-values event)))
      (t
       (let ((value (or (meta-data event key) when-missing)))
	 (unless (eq value :skip)
	   (update! quantity value)))))))

(defmethod find-quantity-class ((spec (eql :meta-data-moments)))
  (find-class 'meta-data-moments))

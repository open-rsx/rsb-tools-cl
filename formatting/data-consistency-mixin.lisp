;;; data-consistency-mixin.lisp --- Ensure consistency of sequential outputs.
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

(cl:in-package :rsb.formatting)

(defclass data-consistency-mixin ()
  ((target-descriptors :type     hash-table
		       :accessor %style-descriptors
		       :initform (make-hash-table :test #'eq)
		       :documentation
		       "Stores a mapping of output target to format
descriptors. All events which are emitted to one target have to
produce matching descriptor."))
  (:documentation
   "This mixin class provides a mechanism for ensuring consistency
between all events which are emitted to a particular target object
like a stream. To achieve this, descriptors are extracted from events
and compared to detect incompatible events."))

(defmethod descriptor-for-target ((style  data-consistency-mixin)
				  (target t))
  (gethash target (%style-descriptors style)))

(defmethod (setf descriptor-for-target) ((new-value t)
					 (style     data-consistency-mixin)
					 (target    t))
  (setf (gethash target (%style-descriptors style)) new-value))

(defmethod compatible-descriptors? ((style        data-consistency-mixin)
				    (descriptor-1 t)
				    (descriptor-2 t))
  (equal descriptor-1 descriptor-2))

(defmethod incompatible-descriptors ((style        data-consistency-mixin)
				     (descriptor-1 t)
				     (descriptor-2 t))
  (error "~@<Data format of current event (~A) is different from the ~
format of previous events (~A).~@:>"
	 descriptor-1 descriptor-2))

(defmethod format-payload :before ((data   t)
				   (style  data-consistency-mixin)
				   (target t)
				   &key &allow-other-keys)
  (let* ((data-descriptor   (make-descriptor style data target))
	 (target-descriptor (descriptor-for-target style target)))
    (cond
      ((null target-descriptor)
       (setf (descriptor-for-target style target) data-descriptor))

      ((not (compatible-descriptors? style target-descriptor data-descriptor))
       (incompatible-descriptors style data-descriptor target-descriptor)))))

(defmethod print-object ((object data-consistency-mixin) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "(~D)" (hash-table-count
			   (%style-descriptors object)))))

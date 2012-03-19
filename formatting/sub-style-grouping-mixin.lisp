;;; sub-style-grouping-mixin.lisp --- Mixin for grouping events into sub-styles.
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

(defclass sub-style-grouping-mixin (delegating-mixin)
  ((key  :initarg  :key
	 :type     function
	 :accessor style-key
	 :initform #'identity
	 :documentation
	 "Stores a function that is called on events to derive a key
objects which identify the sub-style that should be used for the
respective events.")
   (test :initarg  :test
	 :type     function
	 :accessor style-test
	 :initform #'eql
	 :documentation
	 "Stores a function which is used to compare keys when
searching for the sub-style that should be used for a particular
events."))
  (:documentation
   "This mixin class add to `delegating-mixin' the ability to
dynamically create sub-styles and dispatch to these based on
properties of events.

Creation of and dispatching to sub-styles is based on the usual
key/test mechanism for extracting a key from events and testing it
against previously extracted key. A new sub-style is created and added
whenever the key extracted from an event does not match the key of any
previously created sub-styles."))

(defmethod sub-style-for ((style sub-style-grouping-mixin)
			  (event t))
  "If there is a sub-style suitable for handling EVENT, dispatch to
it. Otherwise create a new sub-style and then dispatch to it.  "
  (or (call-next-method)
      (let ((key (style-key style)))
	(push (make-sub-style-entry style (funcall key event))
	      (style-sub-styles style))
	(sub-style-for style event))))

(defmethod format-event :around ((event  t)
				 (style  sub-style-grouping-mixin)
				 (stream t)
				 &key &allow-other-keys)
  (if (eq event :trigger)
      (call-next-method)
      (delegate event style stream)))

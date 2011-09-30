;;; event-style-compact.lisp --- Compact event formatting style class.
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

(defmethod find-style-class ((spec (eql :compact)))
  (find-class 'style-compact))

(defclass style-compact (delegating-mixin
			 columns-mixin
			 header-printing-mixin)
  ()
  (:default-initargs
   :columns    '(:now
		 :origin :sequence-number :id :method :scope :data :data-size
		 :newline)
   :sub-styles `((,#'request-event?
		  .
		  ,(make-instance
		    'columns-mixin
		    :columns '(:now
			       :origin :sequence-number :id :call
			       :newline)))
		 (,#'reply-event?
		  .
		  ,(make-instance
		    'columns-mixin
		    :columns '(:now
			       :origin :sequence-number :call-id :result
			       :newline)))
		 (,#'error-event?
		  .
		  ,(make-instance
		    'columns-mixin
		    :columns '(:now
			       :origin :sequence-number :call-id :result
			       :newline)))
		 (,(constantly t) . :self)))
  (:documentation
   "This formatting style prints several properties of received events
on a single line. Some events are formatted specially according to
their role in a communication pattern."))

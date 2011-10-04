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

(macrolet
    ((define-compact-style (name doc columns sub-styles)
       (let ((class-name (symbolicate "STYLE-" name)))
	`(progn
	   (defmethod find-style-class ((spec (eql ,name)))
	     (find-class ',class-name))

	   (defclass ,class-name (delegating-mixin
				  columns-mixin
				  header-printing-mixin)
	     ()
	     (:default-initargs
	      :columns    ,columns
	      :sub-styles ,sub-styles)
	     (:documentation ,doc))))))

  (define-compact-style :compact
      "This formatting style prints several properties of received
events on a single line. It is designed to fit into 80 columns. Some
events are formatted specially according to their role in a
communication pattern."
    '((:now   :width 26 :alignment :left)
      :origin
      (:scope :width 16)
      (:data  :width 16)
      :data-size
      :newline)
    `((,#'request-event?
       .
       ,(make-instance 'columns-mixin
		       :columns '((:now    :width 26 :alignment :left)
				  :origin
				  (:call   :width 43)
				  :newline)))
      (,#'reply-event?
       .
       ,(make-instance 'columns-mixin
		       :columns '((:now    :width 26 :alignment :left)
				  :origin
				  (:result :width 43)
				  :newline)))
      (,(constantly t) . :self)))

  (define-compact-style :compact+
      "This formatting style prints several properties of received
events on a single line. It is designed to fit into 128 columns. Some
events are formatted specially according to their role in a
communication pattern."
    '(:now
      :origin :sequence-number :id :method :scope :data :data-size
      :newline)
    `((,#'request-event?
       .
       ,(make-instance 'columns-mixin
		       :columns '(:now
				  :origin :sequence-number :id :call :data-size
				  :newline)))
      (,#'reply-event?
       .
       ,(make-instance 'columns-mixin
		       :columns '(:now
				  :origin :sequence-number :call-id :result :data-size
				  :newline)))
      (,(constantly t) . :self))))

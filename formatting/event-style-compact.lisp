;;; event-style-compact.lisp --- Compact event formatting style class.
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


;;; Classes `style-compact/*'
;;
;; These provide increasingly detailed "compact" event formatting.

(macrolet
    ((define-compact-style ((name
			     &key
			     (spec       (make-keyword name))
			     (class-name (symbolicate :style "-" name)))
			    &body doc-and-sub-styles)
       (let+ (((&values sub-styles nil documentation)
	       (parse-body doc-and-sub-styles :documentation t))
	      ((&flet+ make-sub-style ((predicate . spec))
		 `(cons
		   ,predicate
		   (make-instance 'columns-mixin
				  :columns ',spec)))))
	`(progn
	   (defmethod find-style-class ((spec (eql ,spec)))
	     (find-class ',class-name))

	   (defclass ,class-name (delegating-mixin
				  header-printing-mixin)
	     ()
	     (:default-initargs
	      :sub-styles (list ,@(map 'list #'make-sub-style
				       sub-styles)))
	     (:documentation
	      ,(apply #'concatenate 'string
		      "This formatting style prints several properties
of received events on a single line. Some events are formatted
specially according to their role in a communication pattern."
		      (when documentation
			(list " " documentation)))))

	   (defmethod format-header ((style  ,class-name)
				     (stream t))
	     (format-header
	      (cdr (lastcar (style-sub-styles style))) stream))))))

  (define-compact-style (compact/80)
      "The output of this style is designed to fit into 80 columns."
    (#'request-event? . (:now/compact
			 :origin
			 (:call :width 52)
			 :newline))
    (#'reply-event?   . (:now/compact
			 :origin
			 (:result :width 52)
			 :newline))
    ((constantly t)   . (:now/compact
			 :origin
			 (:scope :width 22)
			 (:data :width 22) :data-size
			 :newline)))

  (define-compact-style (compact/128)
      "The output of this style is designed to fit into 128 columns."
    (#'request-event? . (:now/compact
			 :origin :sequence-number :id :call :data-size
			 :newline))
    (#'reply-event?   . (:now/compact
			 :origin :sequence-number :call-id :result :data-size
			 :newline))
    ((constantly t)   . (:now/compact
			 :origin :sequence-number :id :method
			 (:scope :width 29)
			 (:data  :width 34) :data-size
			 :newline)))

  (define-compact-style (compact/180)
      "The output of this style is designed to fit into 180 columns."
    (#'request-event? . (:now
			 :origin :sequence-number :id
			 (:call :width 32) :wire-schema :data-size
			 :newline))
    (#'reply-event?   . (:now
			 :origin :sequence-number :call-id
			 (:result :width 32) :wire-schema :data-size
			 :newline))
    ((constantly t)   . (:now
			 :origin :sequence-number :id :method
			 (:scope :width 32)
			 (:data :width 41) :wire-schema :data-size
			 :newline))))


;;; Class `style-compact'
;;
;; Compact meta-style that dispatches to one of the compact styles
;; based on available horizontal room.

(define-dynamic-width-style (compact)
  ((  0   81) (make-instance 'style-compact/80))
  (( 81  129) (make-instance 'style-compact/128))
  ((129     ) (make-instance 'style-compact/180)))

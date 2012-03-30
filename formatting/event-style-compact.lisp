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


;;; Class `basic-compact' style
;;

(defclass basic-compact-style (delegating-mixin
			       header-printing-mixin)
  ()
  (:documentation
   "This class is intended to be used as a superclass for compact
style classes."))

(defmethod sub-style-for ((style basic-compact-style)
			  (event t))
  "Return a singleton list containing the sub-style of STYLE whose
predicate succeeds on EVENT."
  (ensure-list
   (cdr (find-if (rcurry #'funcall event) (style-sub-styles style)
		 :key #'car))))

(defmethod format-header ((style  basic-compact-style)
			  (stream t))
  (format-header
   (cdr (lastcar (style-sub-styles style))) stream))


;;; Classes `style-compact/*'
;;
;; These provide increasingly detailed "compact" event formatting.

(macrolet
    ((define-compact-style (name &body doc-and-sub-styles)
       (let+ ((class-name (symbolicate :style "-" name))
	      ((&values sub-styles nil documentation)
	       (parse-body doc-and-sub-styles :documentation t))
	      ((&flet+ make-sub-style ((predicate . spec))
		 `(cons
		   ,predicate
		   (make-instance 'columns-mixin
				  :columns ',spec)))))
	`(progn
	   (defmethod find-style-class ((spec (eql ,name)))
	     (find-class ',class-name))

	   (defclass ,class-name (basic-compact-style)
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
			(list " " documentation)))))))))

  (define-compact-style :compact
      "It is designed to fit into 80 columns."
    (#'request-event? . ((:now    :width 26 :alignment :left)
			 :origin
			 (:call   :width 43)
			 :newline))
    (#'reply-event?   . ((:now    :width 26 :alignment :left)
			 :origin
			 (:result :width 43)
			 :newline))
    ((constantly t)   . ((:now    :width 26 :alignment :left)
			 :origin
			 (:scope  :width 16)
			 (:data   :width 16)
			 :data-size
			 :newline)))

  (define-compact-style :compact+
      "It is designed to fit into 128 columns."
    (#'request-event? . (:now
			 :origin :sequence-number :id :call :data-size
			 :newline))
    (#'reply-event?   . (:now
			 :origin :sequence-number :call-id :result :data-size
			 :newline))
    ((constantly t)   . (:now
			 :origin :sequence-number :id :method :scope
			 :data :data-size
			 :newline))))

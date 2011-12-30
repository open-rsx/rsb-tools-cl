;;; columns-mixin.lisp --- Mixin class for column-based formatting styles.
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

(cl:in-package :rsb.formatting)

(eval-when (:compile-toplevel)
  (defmacro when-column-fits ((column separator
			       position
			       produced-output? printed-ellipsis?-var)
			      &body body)
    "Execute BODY if the state captured by POSITION, PRODUCED-OUTPUT?
and PRINTED-ELLIPSIS?-VAR permits printing COLUMN and optionally
SEPARATOR."
    (once-only (position column produced-output? separator)
      `(if (columns-exhausted? (+ ,position
				  (if ,produced-output? (length ,separator) 0)
				  (column-width ,column)))
	   (unless ,printed-ellipsis?-var
	     (format stream ">")
	     (setf ,printed-ellipsis?-var t))
	   (progn ,@body)))))

(defclass columns-mixin ()
  ((columns   :type     list
	      :accessor style-columns
	      :accessor %style-columns ;; does not process column specs
	      :initform nil
	      :documentation
	      "Stores the list of columns of which the formatting
style is composed.")
   (separator :initarg  :separator
	      :type     string
	      :accessor style-separator
	      :initform "|"
	      :documentation
	      "Stores a separator string that is printed between the
output produced by adjacent columns."))
  (:documentation
   "This mixin class is intended to be mixed into formatting styles
that produce column-based output. When combined with
`header-printing-mixin', column names are used to produce header
lines.

When setting columns via the :columns initarg or \(setf
style-columns\), a list of either column instances or column
specifications can be used. A column specification is either a keyword
designating a class in the column class family or a list of the
form

  (CLASS KEY1 VALUE1 KEY2 VALUE2 ...)

consisting of the keyword CLASS and initargs for the column class
designated by CLASS."))

(defmethod shared-initialize :after ((instance   columns-mixin)
                                     (slot-names t)
                                     &key
				     (columns nil columns-supplied?))
  ;; Interpret column specs in COLUMNS, if it has been supplied.
  (when columns-supplied?
    (setf (style-columns instance) columns)))

(defmethod (setf style-columns) :around ((new-value list)
					 (style     columns-mixin))
  ;; Interpret each element of NEW-VALUE as a column instance or a
  ;; specification for creating a column instance.
  (call-next-method (map 'list #'make-column new-value) style))

(defmethod format-header ((style  columns-mixin)
			  (stream t))
  (bind (((:accessors-r/o (columns   style-columns)
			  (separator style-separator)) style)
	 (produced-output?)
	 (printed-ellipsis?))
    (iter (for  column   in columns)
	  (with position =  0)
	  (when (column-produces-output? column)
	    (when-column-fits (column separator
			       position produced-output? printed-ellipsis?)
	      (when produced-output?
		(format stream separator)
		(incf position (length separator)))
	      (with-width-limit (stream (column-width column) :left)
		(format stream "~@(~A~)" (column-name column)))
	      (setf produced-output? t)))
	  (incf position (column-width column))
	  (finally (when produced-output? (terpri stream))))))

(defmethod format-event ((event  t)
			 (style  columns-mixin)
			 (stream t)
			 &key &allow-other-keys)
  (bind (((:accessors-r/o (columns   style-columns)
			  (separator style-separator)) style)
	 (produced-output?)
	 (printed-ellipsis?))
    (iter (for  column   in columns)
	  (with position =  0)
	  (if (column-produces-output? column)
	      (when-column-fits (column separator
				 position produced-output? printed-ellipsis?)
		(when produced-output?
		  (format stream separator)
		  (incf position (length separator)))
		(format-event event column stream)
		(setf produced-output? t))
	      (format-event event column stream))
	  (incf position (column-width column)))))

(defmethod print-object ((object columns-mixin) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "(~D)" (length (style-columns object)))))


;;; Utility function
;;

(defun make-column (spec)
  "Make and return a column instance according to SPEC. SPEC can
either be a keyword, designating a column class, a list of the form

  (CLASS KEY1 VALUE1 KEY2 VALUE2 ...)

designating a column class and specifying initargs, or a column
instance."
  (etypecase spec
    (keyword
     (make-instance (find-column-class spec)))
    (list
     (check-type spec (cons keyword list) "a keyword followed by initargs")
     (bind (((class &rest args) spec))
       (apply #'make-instance (find-column-class class) args)))
    (standard-object
     spec)))

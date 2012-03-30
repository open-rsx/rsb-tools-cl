;;; event-style-monitor.lisp --- Style that aggregates events in sub-styles.
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


;;; Class `basic-monitor-style'
;;

(defclass basic-monitor-style (periodic-printing-mixin
			       sub-style-grouping-mixin
			       sub-style-sorting-mixin
			       header-printing-mixin
			       separator-mixin)
  ()
  (:default-initargs
   :sub-styles       nil

   :sort-predicate   (%make-safe-predicate)
   :sort-key         (%make-column-key-function 2)

   :header-frequency 1

   :separator        :clear)
  (:documentation
   "This class serves as a superclass for formatting style classes
which group events according to some criterion and periodically
display information for events within each group."))

(defmethod format-header ((style  basic-monitor-style)
			  (stream t))
  (unless (emptyp (style-sub-styles style))
    (format-header (cdr (elt (style-sub-styles style) 0)) stream)))

(defmethod format-event ((event  (eql :trigger))
			 (style  basic-monitor-style)
			 (stream t)
			 &key &allow-other-keys)
  (iter (for sub-style each (style-sub-styles/sorted style))
	(format-event event sub-style stream)
	(terpri stream)))


;;; Some concrete monitor styles
;;

(macrolet
    ((define-monitor-style ((kind
			     &rest initargs
			     &key &allow-other-keys)
			    &body doc-and-column-specs)
       (let+ ((spec       (format-symbol :keyword  "~A/~A"
					 :monitor kind))
	      (class-name (format-symbol *package* "~A/~A"
					 :style-monitor kind))
	      ((&values column-specs nil documentation)
	       (parse-body doc-and-column-specs :documentation t))
	      (columns (sublis *basic-columns* column-specs)))
	 `(progn
	    (defmethod find-style-class ((spec (eql ,spec)))
	      (find-class ',class-name))

	    (defclass ,class-name (basic-monitor-style)
	      ()
	      (:default-initargs ,@initargs)
	      ,@(when documentation
		  `((:documentation ,documentation))))

	    (defmethod make-sub-style-entry ((style ,class-name)
					     (value t))
	      (let+ (((&accessors-r/o (key  style-key)
				      (test style-test)) style))
		(cons
		 #'(lambda (event) (funcall test (funcall key event) value))
		 (make-instance 'statistics-columns-mixin
				:columns (list ,@columns)))))))))

  (define-monitor-style (scope
			 :key   #'event-scope
			 :test  #'sub-scope?)
      "This style groups events by scope and periodically displays
various statistics for events in each scope-group."
    (list :constant
	  :name      "Scope"
	  :value     value
	  :formatter #'(lambda (value stream)
			 (write-string (scope-string value) stream))
	  :width     32
	  :alignment :left)
    :rate/12 :throughput/13 :latency :origin/48 :type/48 :size/20)

  (define-monitor-style (origin
			 :key  #'event-origin
			 :test #'uuid:uuid=)
      "This style groups events by origin and periodically displays
various statistics for events in each origin-group. "
    (list :constant
	  :name      "Origin"
	  :value     value
	  :width     36
	  :alignment :left)
    :rate/12 :throughput/13 :latency :scope/46 :type/48 :size/20)

  (define-monitor-style (type
			 :key  #'rsb.stats:event-type/simple
			 :test #'equal)
      "This style groups events by type and periodically displays
various statistics for events in each type-group. "
    (list :constant
	  :name      "Type"
	  :value     value
	  :width     32
	  :alignment :left)
    :rate/12 :throughput/13 :latency :scope/46 :origin/48 :size/20)

  (define-monitor-style (size
			 :key   #'rsb.stats:event-size/power-of-2
			 :test  #'equal)
      "This style groups events by size (each corresponding to a power
of 2) and periodically displays various statistics for events in each
size-group."
    (list :constant
	  :name      "Size"
	  :value     value
	  :width     12
	  :alignment :left)
    :rate/12 :throughput/13 :latency :scope/46 :origin/48 :type/48 :size/20))


;;; Utility functions
;;

(defun %make-safe-predicate (&key
			     (type      'real)
			     (predicate #'>)
			     (fallback  t))
  "Return a function of two arguments which applies PREDICATE for
comparison of both arguments are of type TYPE. If only the first or
second argument is of type TYPE, FALLBACK and (not FALLBACK) are
returned respectively. If neither argument is of type TYPE, `nil' is
returned."
  (declare (type (function (t t) t) predicate))

  #'(lambda (a b)
      (let ((a-ok? (typep a type))
	    (b-ok? (typep b type)))
	(cond
	  ((and a-ok? b-ok?) (funcall predicate a b))
	  (a-ok?             fallback)
	  (b-ok?             (not fallback))
	  (t nil)))))

(defun %make-column-key-function (column)
  "Return a function of one argument, a style object, that extracts
and returns the value of the COLUMN-th column. The column specified by
COLUMN has to be associated with a statistical quantity."
  (compose #'rsb.stats:quantity-value
	   #'column-quantity
	   (rcurry #'elt column)
	   #'style-columns))

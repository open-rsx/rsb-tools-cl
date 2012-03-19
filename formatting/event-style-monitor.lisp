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


;;; Class `basic-monitor'
;;

(defclass basic-monitor (periodic-printing-mixin
			 sub-style-grouping-mixin
			 header-printing-mixin
			 separator-mixin)
  ()
  (:default-initargs
   :sub-styles       nil

   :header-frequency 1

   :separator        :clear)
  (:documentation
   "This class serves as a superclass for formatting style classes
which group events according to some criterion and periodically
display information for events within each group."))

(defmethod format-header ((style  basic-monitor)
			  (stream t))
  (unless (emptyp (style-sub-styles style))
    (format-header (cdr (elt (style-sub-styles style) 0)) stream)))

(defmethod format-event ((event  (eql :trigger))
			 (style  basic-monitor)
			 (stream t)
			 &key &allow-other-keys)
  (iter (for (_ . sub-style) each (style-sub-styles style))
	(format-event event sub-style stream)
	(terpri stream)))


;;; Some concrete monitor styles
;;

(macrolet
    ((define-monitor-style ((kind
			     &key
			     key
			     test)
			    &body doc-and-column-specs)
       (let+ ((spec       (format-symbol :keyword  "~A/~A"
					 :monitor kind))
	      (class-name (format-symbol *package* "~A/~A"
					 :style-monitor kind))
	      ((&values column-specs nil documentation)
	       (parse-body doc-and-column-specs :documentation t)))
	 `(progn
	    (defmethod find-style-class ((spec (eql ,spec)))
	      (find-class ',class-name))

	    (defclass ,class-name (basic-monitor)
	      ()
	      (:default-initargs
	       ,@(when key  `(:key      ,key))
	       ,@(when test `(:test     ,test)))
	      ,@(when documentation
		  `((:documentation ,documentation))))

	    (defmethod make-sub-style-entry ((style ,class-name)
					     (value t))
	      (let+ (((&accessors-r/o (key  style-key)
				      (test style-test)) style)
		     (title (princ-to-string value)))
		(cons
		 #'(lambda (event) (funcall test (funcall key event) value))
		 (make-instance 'statistics-columns-mixin
				:columns (list ,@column-specs)))))))))

  (define-monitor-style (scope
			 :key  #'event-scope
			 :test #'sub-scope?)
      "This style groups events by scope and periodically displays
various statistics for events in each scope-group."
    (list :text :name title :width 32 :alignment :left)
    '(:quantity :quantity :rate       :width 12)
    '(:quantity :quantity :throughput :width 16)
    '(:quantity :quantity (:latency
			   :from :create
			   :to   :deliver
			   :name "Latency"))
    '(:quantity :quantity :origin     :width 48 :alignment :left)
    '(:quantity :quantity :type       :width 48 :alignment :left)
    '(:quantity :quantity :size       :width 20))

  (define-monitor-style (origin
			 :key  #'event-origin
			 :test #'uuid:uuid=)
      "This style groups events by origin and periodically displays
various statistics for events in each origin-group. "
    (list :text :name title :width 32 :alignment :left)
    '(:quantity :quantity :rate       :width 12)
    '(:quantity :quantity :throughput :width 16)
    '(:quantity :quantity (:latency
			   :from :create
			   :to   :deliver
			   :name "Latency"))
    '(:quantity :quantity :scope      :width 48 :alignment :left)
    '(:quantity :quantity :type       :width 48 :alignment :left)
    '(:quantity :quantity :size       :width 20))

  (define-monitor-style (type
			 :key  #'rsb.stats:event-type/simple
			 :test #'equal)
      "This style groups events by type and periodically displays
various statistics for events in each type-group. "
    (list :text :name title :width 32 :alignment :left)
    '(:quantity :quantity :rate       :width 12)
    '(:quantity :quantity :throughput :width 16)
    '(:quantity :quantity (:latency
			   :from :create
			   :to   :deliver
			   :name "Latency"))
    '(:quantity :quantity :scope      :width 48 :alignment :left)
    '(:quantity :quantity :origin     :width 48 :alignment :left)
    '(:quantity :quantity :size       :width 20))

  (define-monitor-style (size
			 :key  #'rsb.stats:event-size/power-of-2
			 :test #'equal)
      "This style groups events by size (each corresponding to a power
of 2) and periodically displays various statistics for events in each
size-group."
    (list :text :name title :width 12 :alignment :left)
    '(:quantity :quantity :rate       :width 12)
    '(:quantity :quantity :throughput :width 16)
    '(:quantity :quantity (:latency
			   :from :create
			   :to   :deliver
			   :name "Latency"))
    '(:quantity :quantity :scope      :width 48 :alignment :left)
    '(:quantity :quantity :origin     :width 48 :alignment :left)
    '(:quantity :quantity :type       :width 48 :alignment :left)
    '(:quantity :quantity :size       :width 20)))

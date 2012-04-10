;;; event-style-timeline.lisp --- Event indicators on a simple timeline.
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


;;; Class `basic-timeline-style'
;;

(defclass basic-timeline-style (basic-monitor-style
				temporal-bounds-mixin)
  ()
  (:default-initargs
   :print-interval .5

   :sort-predicate #'string<
   :sort-key       (compose #'princ-to-string
			    #'column-value
			    (rcurry #'elt 0)
			    #'style-columns))
  (:documentation
   "This class is intended to be used as a superclass for timeline
formatting style classes."))

(macrolet
    ((define-delegating-method (name)
       `(defmethod (setf ,name) :after ((new-value t)
					(style     basic-timeline-style))
	  (iter (for (_ . sub-style) each (style-sub-styles style))
		(setf (,name sub-style) new-value)))))
  (define-delegating-method lower-bound)
  (define-delegating-method upper-bound)
  (define-delegating-method bounds))


;;; Some concrete timeline styles
;;

(macrolet
    ((define-timeline-style ((kind
			     &rest initargs
			     &key &allow-other-keys)
			    &body doc-and-column-specs)
       (let+ ((spec       (format-symbol :keyword  "~A/~A"
					 :timeline kind))
	      (class-name (format-symbol *package* "~A/~A"
					 :style-timeline kind))
	      ((&values column-specs nil documentation)
	       (parse-body doc-and-column-specs :documentation t))
	      (columns column-specs #+no (sublis *basic-columns* column-specs)))
	 `(progn
	    (defmethod find-style-class ((spec (eql ,spec)))
	      (find-class ',class-name))

	    (defclass ,class-name (basic-timeline-style)
	      ()
	      (:default-initargs
	       :columns #'(lambda (value) (list ,@columns))
	       ,@initargs)
	      ,@(when documentation
	         `((:documentation ,documentation))))))))

  (define-timeline-style (scope
			 :key      #'event-scope
			 :test     #'scope=

			 :sort-key (compose #'scope-string
					    #'column-value
					    (rcurry #'elt 0)
					    #'style-columns))
      "This formatting style indicates the points in time at which
events occur as dots on a timeline. Separate \"lanes\" which share a
common timeline are dynamically allocated as events occur. Events are
grouped by scope."
    (list :constant
	  :name      "Scope"
	  :value     value
	  :formatter #'(lambda (value stream)
			 (write-string (scope-string value) stream))
	  :width     33
	  :alignment :left)
    (list :timeline
	  :width     94))

  (define-timeline-style (origin
			 :key  #'event-origin
			 :test #'uuid:uuid=)
      "This formatting style indicates the points in time at which
events occur as dots on a timeline. Separate \"lanes\" which share a
common timeline are dynamically allocated as events occur. Events are
grouped by origin."
    (list :constant
	  :name      "Origin"
	  :value     value
	  :width     36
	  :alignment :left)
    (list :timeline
	  :width     91)))

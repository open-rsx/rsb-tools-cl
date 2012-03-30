;;; event-style-statistics.lisp --- Column-oriented formatting of statistics.
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


;;; Class `statistics-columns-mixin'
;;

(defclass statistics-columns-mixin (columns-mixin)
  ((quantities :type     list
	       :accessor %style-quantities
	       :initform nil
	       :documentation
	       "Stores the list of quantities printed by the
formatting style."))
  (:documentation
   "This class is intended to be mixed into formatting classes that
present the values of statistical quantities in a column-based
manner."))

(defmethod (setf style-columns) :after ((new-value t)
					(style     statistics-columns-mixin))
  ;; Find columns in NEW-VALUE that contain a quantity. Extract and
  ;; store these quantities for direct updates.
  (let+ (((&flet quantity-column? (column)
	    (closer-mop:compute-applicable-methods-using-classes
	     (fdefinition 'column-quantity)
	     (list (class-of column))))))
    (setf (%style-quantities style)
	  (map 'list #'column-quantity
	       (remove-if-not #'quantity-column? (style-columns style))))))

(defmethod format-event :around ((event  t)
				 (style  statistics-columns-mixin)
				 (stream t)
				 &key &allow-other-keys)
  ;; Update quantities.
  (if (eq event :trigger)
      (call-next-method)
      (map nil (rcurry #'rsb.stats:update! event)
	   (%style-quantities style))))


;;; Classes `style-statistics/*'
;;
;; These provide increasingly detailed statistics formatting.

(macrolet
    ((define-statistics-style ((name
				&key
				(spec       (make-keyword name))
				(class-name (symbolicate :style "-" name)))
			       &body doc-and-column-specs)
       (let+ (((&values column-specs nil documentation)
	       (parse-body doc-and-column-specs :documentation t)))
	 `(progn
	    (defmethod find-style-class ((spec (eql ,spec)))
	      (find-class ',class-name))

	    (defclass ,class-name (periodic-printing-mixin
				   statistics-columns-mixin
				   header-printing-mixin)
	      ()
	      (:default-initargs
	       :columns (list ,@(sublis *basic-columns* column-specs)))
	      (:documentation
	       ,(apply #'concatenate 'string
		      "This formatting style computes a number of
configurable statistical quantities from received events collected
over a configurable period of time and prints the computed values in a
tabular manner."
		      (when documentation
			(list " " documentation)))))))))

  (define-statistics-style (statistics/80)
      "The output of this style is designed to fit into 80 columns."
    :now/compact :rate/12 :throughput/13 :latency :size/20
    :newline)

  (define-statistics-style (statistics/128)
      "The output of this style is designed to fit into 128 columns."
    :now/compact :rate/12 :throughput/13 :latency :scope/46 :size/20
    :newline)

  (define-statistics-style (statistics/180)
      "The output of this style is designed to fit into 180 columns."
    :now :rate/12 :throughput/13 :latency :origin/35 :scope/46 :size/20
    :newline)

  (define-statistics-style (statistics/220)
      "The output of this style is designed to fit into 180 columns."
    :now :rate/12 :throughput/13 :latency :origin/35 :scope/46 :type/39 :size/20
    :newline))


;;; Class `style-statistics'
;;
;; Statistics meta-style that dispatches to one of the statistics
;; styles based on available horizontal room.

(define-dynamic-width-style (statistics
			     :superclasses (periodic-printing-mixin))
  ((  0   81) (make-instance 'style-statistics/80 :print-interval nil))
  (( 81  129) (make-instance 'style-statistics/128 :print-interval nil))
  ((129  181) (make-instance 'style-statistics/180 :print-interval nil))
  ((181     ) (make-instance 'style-statistics/220 :print-interval nil)))

;;;; event-style-statistics.lisp --- Column-oriented formatting of statistics.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.formatting)

;;; Class `statistics-columns-mixin'

(defclass statistics-columns-mixin (columns-mixin)
  ()
  (:documentation
   "This class is intended to be mixed into formatting classes that
    present the values of statistical quantities in a column-based
    manner."))

(defmethod collects? ((style statistics-columns-mixin))
  t)

(defmethod format-event :around ((event  t)
                                 (style  statistics-columns-mixin)
                                 (stream t)
                                 &key &allow-other-keys)
  ;; Update quantities.
  (if (eq event :trigger)
      (call-next-method)
      (map nil (lambda (column)
                 (when (collects? column)
                   (format-event event column stream)))
           (style-columns style))))

;;; Class `style-statistics'

(defmethod find-style-class ((spec (eql :statistics)))
  (find-class 'style-statistics))

(defclass style-statistics (periodic-printing-mixin
                            statistics-columns-mixin
                            widths-caching-mixin
                            header-printing-mixin)
  ()
  (:default-initargs
   :columns (sublis *basic-columns*
                    '(:now :rate/12 :throughput/13 :latency :scope/40
                      :type/40 :size/20 :origin/40 :newline)))
  (:documentation
   "This formatting style computes a number of configurable
    statistical quantities from received events collected over a
    configurable period of time and prints the computed values in a
    tabular manner."))

(defmethod format-event :before ((event  (eql :trigger))
                                 (style  style-statistics)
                                 (stream t)
                                 &key
                                 (width (or *print-right-margin* 80))
                                 &allow-other-keys)
  (let* ((columns   (style-dynamic-width-columns style))
         (separator (style-separator-width       style))
         (widths    (style-compute-column-widths
                     style columns width :separator-width separator)))
    (style-assign-column-widths style columns widths)))

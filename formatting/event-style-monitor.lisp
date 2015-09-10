;;;; event-style-monitor.lisp --- Style that aggregates events in sub-styles.
;;;;
;;;; Copyright (C) 2012, 2013, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.formatting)

;;; Class `basic-monitor-line-style'

(defclass basic-monitor-line-style (activity-tracking-mixin
                                    statistics-columns-mixin)
  ()
  (:documentation
   "Instances of this class are individual lines in the output of a
    monitor style.

    Lines consist of columns which are mostly statistical
    quantities. Activity of lines is tracked, allowing the containing
    style to drop lines after a period of inactivity."))

;;; Class `basic-monitor-style'

(defclass monitor-style-mixin (output-buffering-mixin
                               periodic-printing-mixin
                               sub-style-grouping-mixin
                               activity-based-sub-style-pruning-mixin
                               widths-caching-mixin
                               separator-mixin
                               header-printing-mixin)
  ((columns          :initarg    :columns
                     :type       function
                     :reader     style-columns
                     :documentation
                     "Stores a specification for creating columns used
                      by sub-styles of the style.")
   (line-style-class :initarg  :line-style-class
                     :reader   style-line-style-class
                     :documentation
                     "Stores the name of a class that should be used
                      when making instances for use as line styles."))
  (:default-initargs
   :sub-styles       nil

   :header-frequency 1

   :separator        :clear

   :columns          (missing-required-initarg 'monitor-style-mixin :columns)
   :line-style-class 'basic-monitor-line-style)
  (:documentation
   "This class is intended to be mixed into formatting style classes
    which group events according to some criterion and periodically
    display information for events within each group in one line of
    text."))

(defmethod make-sub-style-entry ((style monitor-style-mixin)
                                 (value t))
  (let+ (((&structure-r/o style- key test columns line-style-class) style))
    (declare (type function key test columns))
    (when-let ((columns (funcall columns value)))
      (cons (lambda (event) (funcall test (funcall key event) value))
            (make-instance line-style-class :columns columns)))))

(defmethod style-dynamic-width-columns ((style monitor-style-mixin))
  ;; Return columns of first sub-style (i.e. first line) for
  ;; width-related computations.
  (unless (emptyp (style-sub-styles style))
    (style-dynamic-width-columns (cdr (first-elt (style-sub-styles style))))))

(defmethod format-header ((style  monitor-style-mixin)
                          (stream t))
  ;; Use first sub-style (i.e first line) to format the column
  ;; headers.
  (unless (emptyp (style-sub-styles style))
    (format-header (cdr (first-elt (style-sub-styles style))) stream)))

(defmethod format-event :before ((event  (eql :trigger))
                                 (style  monitor-style-mixin)
                                 (stream t)
                                 &key
                                 (width (or *print-right-margin* 80))
                                 &allow-other-keys)
  ;; Before displaying sub-styles, compute widths based on first
  ;; sub-style (i.e. first line), then propagate to remaining
  ;; sub-styles.
  (let ((sub-styles (mapcar #'cdr (style-sub-styles style))))
    (unless (emptyp sub-styles)
      (let* ((sub-style (first-elt sub-styles))
             (columns   (style-dynamic-width-columns sub-style))
             (separator (style-separator-width       sub-style))
             (widths    (style-compute-column-widths
                         style columns width :separator-width separator)))
        (iter (for sub-style in-sequence sub-styles)
              (let ((columns (style-dynamic-width-columns sub-style)))
                (style-assign-column-widths sub-style columns widths)))))))

(defmethod format-event ((event  (eql :trigger))
                         (style  monitor-style-mixin)
                         (stream t)
                         &key &allow-other-keys)
  ;; Print all sub-styles (i.e. lines), separated by newlines.
  (iter (for (_ . sub-style) in-sequence (style-sub-styles style))
        (format-event event sub-style stream)
        (terpri stream)))

;;; `sorted-monitor-style'

(defclass sorted-monitor-style (monitor-style-mixin
                                sub-style-sorting-mixin)
  ()
  (:default-initargs
   :sort-predicate #'column<
   :sort-key       (%make-column-key-function 0))
  (:documentation
   "This class serves as a superclass for formatting style classes
    which group events according to some criterion and periodically
    display information for events within each group as a sorted list
    of text lines."))

(defmethod shared-initialize :after ((instance   sorted-monitor-style)
                                     (slot-names t)
                                     &key
                                     (sort-column   nil sort-column-supplied?)
                                     (sort-reverse? nil sort-reverse?-supplied?))
  (when sort-column-supplied?
    (setf (style-sort-key instance) (%make-column-key-function sort-column)))
  (when sort-reverse?-supplied?
    (setf (style-sort-predicate instance)
          (if sort-reverse?
              (complement #'column<)
              #'column<))))

(defmethod format-event ((event  (eql :trigger))
                         (style  sorted-monitor-style)
                         (stream t)
                         &key &allow-other-keys)
  ;; Like `monitor-style-mixin', but display sorted sequence of
  ;; sub-styles.
  (iter (for sub-style in-sequence (style-sub-styles/sorted style))
        (format-event event sub-style stream)
        (terpri stream)))

;;; Some concrete monitor styles

(macrolet
    ((define-monitor-style ((kind &rest initargs &key &allow-other-keys)
                            &body doc-and-column-specs)
       (let+ ((spec       (format-symbol :keyword  "~A/~A"
                                         :monitor kind))
              (class-name (format-symbol *package* "~A/~A"
                                         :style-monitor kind))
              ((&values column-specs nil documentation)
               (parse-body doc-and-column-specs :documentation t)))
         `(progn
            (defclass ,class-name (sorted-monitor-style)
              ()
              (:default-initargs
               :columns (lambda (value)
                          (sublis *basic-columns* (list ,@column-specs)))
               ,@initargs)
              ,@(when documentation
                  `((:documentation ,documentation))))

            (service-provider:register-provider/class
             'style ,spec :class ',class-name)))))

  (define-monitor-style (scope :key #'event-scope :test #'scope=)
    "This style groups events by scope and periodically displays
     various statistics for events in each scope-group."
    ;; Specification for group column.
    (list :constant
          :name      "Scope"
          :value     value
          :formatter (lambda (value stream)
                       (write-string (scope-string value) stream))
          :widths    '(:range 38)
          :priority  3.2
          :alignment :left)
    ;; Specifications for remaining columns.
    :rate :throughput :latency :timeline :type/40 :size :origin/40)

  (service-provider:register-provider/class ; alias
   'style :monitor/timeline :class 'style-monitor/scope)

  (service-provider:register-provider/class ; alias
   'style :monitor :class 'style-monitor/scope)

  (define-monitor-style (origin :key #'event-origin :test #'uuid:uuid=)
    "This style groups events by origin and periodically displays
     various statistics for events in each origin-group."
    ;; Specification for group column.
    (list :constant
          :name      "Origin"
          :value     value
          :width     38
          :alignment :left)
    ;; Specifications for remaining columns.
    :rate :throughput :latency :scope/40 :timeline :type/40 :size)

  (define-monitor-style (type
                         :key  #'rsb.stats:event-type/simple
                         :test #'equal)
    "This style groups events by type and periodically displays
     various statistics for events in each type-group."
    ;; Specification for group column.
    (list :constant
          :name      "Type"
          :value     value
          :widths    '(:range 35)
          :alignment :left)
    ;; Specifications for remaining columns.
    :rate :throughput :latency :scope/40 :timeline :size :origin/40)

  (define-monitor-style (size
                         :key  #'rsb.stats:event-size/power-of-2
                         :test #'equal)
    "This style groups events by size (each corresponding to a power
     of 2) and periodically displays various statistics for events in
     each size-group."
    ;; Specification for group column.
    (list :constant
          :name      "Size"
          :value     value
          :formatter (lambda (value stream)
                       (rsb.formatting:print-human-readable-size stream value))
          :width     5
          :alignment :left)
    ;; Specifications for remaining columns.
    :rate :throughput :latency :scope/40 :timeline :type/40 :size :origin/40))

;;; Utility functions

(defun %make-column-key-function (column)
  "Return a function of one argument, a style object, that extracts
   and returns the value of the COLUMN-th column. The column specified
   by COLUMN has to be associated with a statistical quantity."
  (lambda (style)
    (let ((columns (style-columns style)))
      (elt columns (min column (1- (length columns)))))))

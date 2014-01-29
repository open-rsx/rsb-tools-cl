;;;; event-style-monitor.lisp --- Style that aggregates events in sub-styles.
;;;;
;;;; Copyright (C) 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.formatting)

;;; Class `basic-monitor-style'

(defclass basic-monitor-style (output-buffering-mixin
                               periodic-printing-mixin
                               sub-style-grouping-mixin
                               sub-style-sorting-mixin
                               separator-mixin
                               header-printing-mixin)
  ((columns :initarg    :columns
            :type       function
            :reader     style-columns
            :documentation
            "Stores a specification for creating columns used by
             sub-styles of the style."))
  (:default-initargs
   :sub-styles       nil

   :sort-predicate   (%make-safe-predicate)
   :sort-key         (%make-column-key-function 2)

   :header-frequency 1

   :separator        :clear

   :columns          (missing-required-initarg
                      'basic-monitor-style :columns))
  (:documentation
   "This class serves as a superclass for formatting style classes
    which group events according to some criterion and periodically
    display information for events within each group."))

(defmethod make-sub-style-entry ((style basic-monitor-style)
                                 (value t))
  (let+ (((&accessors-r/o (key     style-key)
                          (test    style-test)
                          (columns style-columns)) style))
    (declare (type function key test columns))
    (when-let ((columns (funcall columns value)))
      (cons
       (lambda (event) (funcall test (funcall key event) value))
       (make-instance 'statistics-columns-mixin
                      :columns columns)))))

(defmethod format-header ((style  basic-monitor-style)
                          (stream t))
  (unless (emptyp (style-sub-styles style))
    (format-header (cdr (elt (style-sub-styles style) 0)) stream)))

(defmethod format-event ((event  (eql :trigger))
                         (style  basic-monitor-style)
                         (stream t)
                         &key &allow-other-keys)
  (iter (for sub-style in-sequence (style-sub-styles/sorted style))
        (format-event event sub-style stream)
        (terpri stream)))

;;; Some concrete monitor styles

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
              (:default-initargs
               :columns (lambda (value) (list ,@columns))
               ,@initargs)
              ,@(when documentation
                  `((:documentation ,documentation)))))))

     (define-dynamic-width-monitor-style ((kind &rest args)
                                          &body doc-and-specs)
       (let+ (((&values (group-column-spec &rest other-specs)
                        nil documentation)
               (parse-body doc-and-specs :documentation t))
              (name (format-symbol *package* "~A/~A" :monitor kind))
              (dispatch-specs)
              ((&flet+ do-sub-style (((min &optional max) &rest spec))
                 (let* ((name       (format-symbol
                                     *package* "~A/~:[*~;~:*~D~]"
                                     kind (when max (1- max))))
                        (class-name (format-symbol
                                     *package* "~A/~A"
                                     :style-monitor name)))
                   (appendf dispatch-specs
                            `(((,min ,@(when max `(,max)))
                               (make-instance ',class-name
                                              :print-interval nil))))
                   `(define-monitor-style (,name ,@args)
                        ,(format nil "~A The output of this style is ~
                                      designed to fit into ~:[~D or ~
                                      more columns~;~:*~D columns~]."
                                 documentation (when max (1- max)) min)
                      ,group-column-spec
                      ,@spec)))))
         `(progn
            ,@(map 'list #'do-sub-style other-specs)

            (define-dynamic-width-style (,name
                                         :superclasses (periodic-printing-mixin))
              ,@dispatch-specs)))))

  (define-dynamic-width-monitor-style (scope
                                       :key  #'event-scope
                                       :test #'scope=)
      "This style groups events by scope and periodically displays
       various statistics for events in each scope-group."
    ;; Specification for group column.
    (list :constant
          :name      "Scope"
          :value     value
          :formatter (lambda (value stream)
                       (write-string (scope-string value) stream))
          :width     32
          :alignment :left)
    ;; Width-dependent specifications for remaining columns.
    ((  0  81) :rate/12 :throughput/13 :latency)
    (( 81 129) :rate/12 :throughput/13 :latency :size/20)
    ((129 181) :rate/12 :throughput/13 :latency :type/40 :size/20)
    ((181    ) :rate/12 :throughput/13 :latency :origin/40 :type/40 :size/20))

  (defmethod find-style-class ((spec (eql :monitor)))
    (find-style-class :monitor/scope))

  (define-dynamic-width-monitor-style (origin
                                       :key  #'event-origin
                                       :test #'uuid:uuid=)
      "This style groups events by origin and periodically displays
       various statistics for events in each origin-group."
    ;; Specification for group column.
    (list :constant
          :name      "Origin"
          :value     value
          :width     36
          :alignment :left)
    ;; Width-dependent specifications for remaining columns.
    ((  0  81) :rate/12 :throughput/13 :latency)
    (( 81 129) :rate/12 :throughput/13 :latency :size/20)
    ((129 181) :rate/12 :throughput/13 :latency :type/40 :size/20)
    ((181    ) :rate/12 :throughput/13 :latency :scope/40 :type/40 :size/20))

  (define-dynamic-width-monitor-style
      (type
       :key  #'rsb.stats:event-type/simple
       :test #'equal)
      "This style groups events by type and periodically displays
       various statistics for events in each type-group."
    ;; Specification for group column.
    (list :constant
          :name      "Type"
          :value     value
          :width     32
          :alignment :left)
    ;; Width-dependent specifications for remaining columns.
    ((  0  81) :rate/12 :throughput/13 :latency)
    (( 81 129) :rate/12 :throughput/13 :latency :size/20)
    ((129 181) :rate/12 :throughput/13 :latency :scope/40 :size/20)
    ((181    ) :rate/12 :throughput/13 :latency :scope/40 :origin/40 :size/20))

  (define-dynamic-width-monitor-style
      (size
       :key  #'rsb.stats:event-size/power-of-2
       :test #'equal)
      "This style groups events by size (each corresponding to a power
       of 2) and periodically displays various statistics for events
       in each size-group."
    ;; Specification for group column.
    (list :constant
          :name      "Size"
          :value     value
          :width     12
          :alignment :left)
    ;; Width-dependent specifications for remaining columns.
    ((  0  81) :rate/12 :throughput/13 :latency)
    (( 81 129) :rate/12 :throughput/13 :latency :type/40)
    ((129 181) :rate/12 :throughput/13 :latency :scope/40 :origin/40 :type/40)
    ((181    ) :rate/12 :throughput/13 :latency :scope/40 :origin/40 :type/40 :size/20)))

;;; Utility functions

(defun %make-safe-predicate (&key
                             (type      'real)
                             (predicate #'>)
                             (fallback  t))
  "Return a function of two arguments which applies PREDICATE for
   comparison of both arguments are of type TYPE. If only the first or
   second argument is of type TYPE, FALLBACK and (not FALLBACK) are
   returned respectively. If neither argument is of type TYPE, `nil'
   is returned."
  (declare (type (function (t t) t) predicate))

  (lambda (a b)
    (let ((a-ok? (typep a type))
          (b-ok? (typep b type)))
      (cond
        ((and a-ok? b-ok?) (funcall predicate a b))
        (a-ok?             fallback)
        (b-ok?             (not fallback))
        (t                 nil)))))

(defun %make-column-key-function (column)
  "Return a function of one argument, a style object, that extracts
   and returns the value of the COLUMN-th column. The column specified
   by COLUMN has to be associated with a statistical quantity."
  (compose #'rsb.stats:quantity-value
           #'column-quantity
           (rcurry #'elt column)
           #'style-columns))

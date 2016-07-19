;;;; event-style-monitor.lisp --- Style that aggregates events in sub-styles.
;;;;
;;;; Copyright (C) 2012, 2013, 2014, 2015, 2016 Jan Moringen
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
  ((columns          :type       function
                     :reader     style-columns
                     :accessor   style-%columns
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

   :line-style-class 'basic-monitor-line-style)
  (:documentation
   "This class is intended to be mixed into formatting style classes
    which group events according to some criterion and periodically
    display information for events within each group in one line of
    text."))

(defmethod initialize-instance :before ((instance monitor-style-mixin)
                                        &key
                                        (columns         nil columns-supplied?)
                                        (default-columns nil default-columns-supplied?))
  (declare (ignore columns default-columns))
  (unless (or columns-supplied? default-columns-supplied?)
    (missing-required-initarg
     'monitor-style-mixin :columns-or-default-columns)))

(defmethod shared-initialize :after
    ((instance   monitor-style-mixin)
     (slot-names t)
     &key
     (default-columns nil             default-columns-supplied?)
     (columns         default-columns columns-supplied?))
  (when (or columns-supplied? default-columns-supplied?)
    (let+ (((&flet make-generator (specs)
              (lambda (value)
                (mapcar (lambda (spec)
                          (if (functionp spec)
                              (funcall spec value)
                              spec))
                        specs)))))
      (setf (style-%columns instance)
            (cond
              ((not columns-supplied?)
               (make-generator default-columns))
              ((functionp columns)
               columns)
              (t
               (make-generator
                (list* (first default-columns)
                       (mapcar #'expand-column-spec columns)))))))))

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
                                 (width (or *print-right-margin* 80)))
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
                         &key)
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
                         &key)
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
               :default-columns (mapcar #'expand-column-spec
                                        (list ,@column-specs))
               ,@initargs)
              ,@(when documentation
                  `((:documentation ,documentation))))

            (service-provider:register-provider/class
             'style ,spec :class ',class-name)))))

  (define-monitor-style (scope/flat :key #'event-scope :test #'scope=)
    "This style groups events by scope and periodically displays
     various statistics for events in each scope-group."
    ;; Specification for group column.
    (lambda (value)
      (list :constant
            :name      "Scope"
            :value     value
            :formatter (lambda (value stream)
                         (write-string (scope-string value) stream))
            :widths    '(:range 38)
            :priority  3.2
            :alignment :left))
    ;; Specifications for remaining columns.
    :rate :throughput :latency :timeline :type/40 :size :origin/40 :method/20)

  (service-provider:register-provider/class ; alias
   'style :monitor/timeline :class 'style-monitor/scope/flat)

  (define-monitor-style (origin :key #'event-origin :test #'uuid:uuid=)
    "This style groups events by origin and periodically displays
     various statistics for events in each origin-group."
    ;; Specification for group column.
    (lambda (value)
      (list :constant
            :name      "Origin"
            :value     value
            :width     38
            :alignment :left))
    ;; Specifications for remaining columns.
    :rate :throughput :latency :scope/40 :timeline :type/40 :size :method/20)

  (define-monitor-style (type
                         :key  #'rsb.stats:event-type/simple
                         :test #'equal)
    "This style groups events by type and periodically displays
     various statistics for events in each type-group."
    ;; Specification for group column.
    (lambda (value)
      (list :constant
            :name      "Type"
            :value     value
            :widths    '(:range 35)
            :alignment :left))
    ;; Specifications for remaining columns.
    :rate :throughput :latency :scope/40 :timeline :size :origin/40 :method/20)

  (define-monitor-style (size
                         :key  #'rsb.stats:event-size/power-of-2
                         :test #'equal)
    "This style groups events by size (each corresponding to a power
     of 2) and periodically displays various statistics for events in
     each size-group."
    ;; Specification for group column.
    (lambda (value)
      (list :constant
            :name      "Size"
            :value     value
            :formatter (lambda (value stream)
                         (rsb.formatting:print-human-readable-size stream value))
            :width     5
            :alignment :left))
    ;; Specifications for remaining columns.
    :rate :throughput :latency :scope/40 :timeline :type/40 :size :origin/40 :method/20))

;;; Scope-tree monitor style
;;;
;;; Uses a tree of scopes to group sub-styles.

(defclass monitor-line-style/tree (basic-monitor-line-style)
  ((parent   :initarg  :parent
             :accessor style-parent
             :initform nil)
   (children :type     list
             :accessor style-children
             :initform '()))
  (:documentation
   "Instances of this class are lines in the output of a tree-oriented
    monitor style.

    As with the superclass, `basic-monitor-line-style', lines contain
    columns. To represent the tree structure, this class adds a list
    of child lines."))

(defclass style-monitor/scope/tree (sorted-monitor-style)
  ((max-depth        :initarg  :max-depth
                     :type     non-negative-integer
                     :reader   style-max-depth
                     :initform 3)
   (collapse-scopes? :initarg  :collapse-scopes?
                     :type     boolean
                     :reader   style-collapse-scopes?
                     :initform t
                     :documentation
                     "Controls whether lines corresponding to chains
                      of scopes with exactly one known sub-scope are
                      collapsed into a single line.")
   (children         :type     (or null (cons t null))
                     :accessor style-children
                     :initform '())
   (cache            :type     hash-table
                     :accessor style-%cache
                     :initform (make-hash-table :test #'equal)))
  (:default-initargs
   :key              #'event-scope
   :test             #'scope=
   :default-columns  (mapcar #'expand-column-spec
                             (list (lambda (value)
                                     (%make-scope-suffix-column value))
                                   :rate :throughput :latency :timeline
                                   :type/40 :size :origin/40 :method/20))
   :line-style-class 'monitor-line-style/tree)
  (:documentation
   "This style groups events according to the tree formed by their
    scopes and periodically displays various statistics for events in
    each scope-group.

    The depth of the tree can be limited via the max-depth initarg and
    collapsing of scopes with only a single sub-scope can be
    controlled via the collapse-scopes? initarg."))

;; Name and aliases
(dolist (name '(:monitor/scope/tree :monitor/scope :monitor))
  (service-provider:register-provider/class
   'style name :class 'style-monitor/scope/tree))

(defmethod style-parent ((style style-monitor/scope/tree))
  nil)

(defmethod sub-style-for ((style style-monitor/scope/tree) (event t))
  (let+ (((&structure-r/o style- key max-depth %cache) style)
         ((&flet parent-scope-event (components)
            (make-event (make-scope (butlast components)) nil)))
         ;; Traverse super-scopes up to root creating sub-styles along
         ;; the way as necessary.
         ((&labels one-component (event)
            (let* ((scope      (funcall key event))
                   (components (scope-components scope))
                   (sub-style  (unless (> (length components) max-depth)
                                 (first (call-next-method style event))))
                   (ancestors  (unless (scope= scope rsb::+root-scope+)
                                 (one-component (parent-scope-event
                                                 components))))
                   (parent     (or (first ancestors) style)))
              (cond
                ((not sub-style))
                (parent
                 (setf (style-parent sub-style) parent)
                 (pushnew sub-style (style-children parent) :test #'eq)))
              (list* sub-style ancestors))))
         (components (scope-components (funcall key event))))
    ;; If possible, use cached sub-style list. The cache is flushed
    ;; when sub-styles are pruned.
    (ensure-gethash components %cache (one-component event))))

(defmethod prune-sub-styles :around ((style style-monitor/scope/tree))
  (let+ ((old-value (style-sub-styles style))
         (new-value (progn
                      (call-next-method)
                      (style-sub-styles style)))
         (removed   (set-difference old-value new-value :test #'eq))
         ((&flet remove-from-parent (child)
            (when-let* ((child  (cdr child))
                        (parent (style-parent child)))
              (removef (style-children parent) child :test #'eq)))))
    ;; Since removal of sub-styles invalidates the cache, flush it.
    (when removed
      (clrhash (style-%cache style)))
    (mapc #'remove-from-parent removed)))

(declaim (special *suffix-length*))

(defmethod format-event ((event  (eql :trigger))
                         (style  style-monitor/scope/tree)
                         (target stream)
                         &key)
  (let+ (((&structure-r/o style- sort-predicate sort-key collapse-scopes?)
          style)
         ((&flet+ print-first-line (stream depth (collapsed-count . node))
            ;; Columns widths are adjusted prior to each redisplay. It
            ;; is therefore OK to mutate them here. Specifically, we
            ;; take away from the respective first column (contains
            ;; scope) the width consumed by the tree structure
            ;; indentation.
            (decf (column-width (first (style-columns node))) (* 2 depth))
            ;; Bind `*suffix-length*' for appropriate scope suffix
            ;; printing in case singleton nodes have been collapsed.
            (let ((*suffix-length* (1+ collapsed-count)))
              (format-event event node stream))))
         ;; Recursively replace "singleton" nodes (nodes that have
         ;; exactly one child) with the child, keeping track of the
         ;; length of the replacement chain.
         ((&labels collapse-singletons (node &optional (collapsed-count 0))
            (let ((children (style-children node)))
              (if (and collapse-scopes? (length= 1 children))
                  (collapse-singletons (first children) (1+ collapsed-count))
                  (cons collapsed-count node)))))
         ((&flet+ node-children ((&ign . node))
            (let ((children (mapcar #'collapse-singletons
                                    (style-children node))))
              (sort children sort-predicate :key (compose sort-key #'cdr))))))
    (when-let ((root (first (style-children style))))
      (utilities.print-tree:print-tree
       target (collapse-singletons root)
       (utilities.print-tree:make-node-printer
        #'print-first-line nil #'node-children))))
  (terpri target))

;;; Utility functions

(defvar *suffix-length* 1
  "Length of the scope suffix that should be printed.

   Values greater than 1 arise when \"singleton\" nodes (nodes with
   exactly one child) are collapsed.")

(defun %make-scope-suffix-column (scope)
  (list :constant
        :name      "Scope"
        :value     scope
        :formatter (lambda (scope stream)
                     ;; `suffix-length' is greater than the number of
                     ;; components iff either the root scope has been
                     ;; collapsed or SCOPE is the root scope.
                     (let ((components    (scope-components scope))
                           (suffix-length *suffix-length*))
                       (format stream "~:[~;/~]~{~A~^/~}"
                               (> suffix-length (length components))
                               (last components suffix-length))))
        :widths    '(:range 16)
        :priority  2.2
        :alignment :left))

(defun %make-column-key-function (column)
  "Return a function of one argument, a style object, that extracts
   and returns the value of the COLUMN-th column. The column specified
   by COLUMN has to be associated with a statistical quantity."
  (lambda (style)
    (let ((columns (style-columns style)))
      (elt columns (min column (1- (length columns)))))))

;;;; protocol.lisp --- Protocol for formatting of RSB events.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.formatting)

;;; Event formatting protocol

(defgeneric format-event (event style stream
                          &key
                          max-lines
                          max-columns)
  (:documentation
   "Format EVENT onto STREAM using a style designated by STYLE.
    MAX-LINES controls specifies the maximum number of lines the
    produced output is allowed to take up.  MAX-COLUMNS limits the
    number of columns individual output lines are allowed to take
    up."))

(defgeneric format-payload (data style stream
                            &key
                            max-lines
                            max-columns)
  (:documentation
   "Format the event payload DATA onto STREAM using a formatting style
    designated by STYLE.

    MAX-LINES controls specifies the maximum number of lines the
    produced output is allowed to take up.

    MAX-COLUMNS limits the number of columns individual output lines
    are allowed to take up."))

;;; Formatting style class family

(dynamic-classes:define-findable-class-family style
  "This class family consists of event formatting style classes. Each
   class implements a particular style of formatting received events
   onto a given stream by specializing `format-event'.")

(defun make-style (spec)
  "Make and return a style instance according to SPEC. SPEC can either
   be a keyword, designating a style class, a list of the form

     (CLASS KEY1 VALUE1 KEY2 VALUE2 ...)

   designating a style class and specifying initargs, or a style
   instance."
  (etypecase spec
    (keyword
     (make-instance (find-style-class spec)))
    (list
     (check-type spec (cons keyword list) "a keyword followed by initargs")
     (let+ (((class &rest args) spec))
       (apply #'make-instance (find-style-class class) args)))
    (standard-object
     spec)))

;;; Collecting protocol

(defgeneric collects? (thing)
  (:documentation
   "Return non-nil if THING collects data from events and only
    produces output for explicit trigger events."))

;; Default behavior

(defmethod collects? ((thing t))
  nil)

;;; Delegation protocol

(defgeneric sub-style-for (style event)
  (:documentation
   "Return a sub-style object of STYLE or a sequence of such style
    objects for formatting EVENT. nil indicates that EVENT should not
    be processed in any sub-style."))

(defgeneric delegate (event style stream)
  (:documentation
   "Delegate processing of EVENT on STREAM by STYLE to a sub-style."))

(defgeneric make-sub-style-entry (style value)
  (:documentation
   "Create and return a list of the form

      (PREDICATE SUB-STYLE)

    where PREDICATE is a function of one parameter, a thing to be
    formatted, which decides whether the sub-style SUB-STYLE of STYLE
    is suitable for formatting the object. Return nil when no
    sub-style entry should be created for VALUE. VALUE depends on how
    STYLE organizes its sub-styles. See `sub-style-for' and
    `delegate'."))

;;; Sub-style sorting protocol

(defgeneric style-sub-styles/sorted (style
                                     &key
                                     predicate
                                     key)
  (:documentation
   "Return a list of style object which are sub-styles of STYLE,
    sorted according to PREDICATE and KEY."))

;;; Sub-style pruning protocol

(defgeneric style-prune-predicate (style)
  (:documentation
   "Return the prune predicate of STYLE.

    Either nil or a function accepting a sub-style object and
    returning true if the sub-style should be pruned."))

(defgeneric (setf style-prune-predicate) (new-value style)
  (:documentation
   "Set prune predicate of STYLE to NEW-VALUE.

    NEW-VALUE is either nil or a function accepting a sub-style object
    and returning true if the sub-style should be pruned."))

(defgeneric prune-sub-styles (style)
  (:documentation
   "Remove sub-styles satisfying the prune predicate of STYLE (if any)
    from STYLE."))

;;; Data consistency protocol

(defgeneric descriptor-for-target (style target)
  (:documentation
   "Return a descriptor of acceptable output send to TARGET by STYLE.

    The returned descriptor can be any object returned by a method for
    STYLE on `make-descriptor' that works with specialized methods for
    STYLE on `compatible-descriptors?' and
    `incompatible-descriptors'."))

(defgeneric (setf descriptor-for-target) (new-value style target)
  (:documentation
   "Install NEW-VALUE as an descriptor of acceptable output send to
    TARGET by STYLE.

    NEW-VALUE can be any object returned by a method for STYLE on
    `make-descriptor' that works with specialized methods for STYLE on
    `compatible-descriptors?' and `incompatible-descriptors'."))

(defgeneric make-descriptor (style data target)
  (:documentation
   "Return a descriptor of acceptable output send to TARGET by
    STYLE. The descriptor can be derived from DATA when future output
    has to be compatible to DATA in some sense.

    The returned descriptor has to work with methods on
    `compatible-descriptors?' and `incompatible-descriptors'
    specialized for STYLE."))

(defgeneric compatible-descriptors? (style descriptor-1 descriptor-2)
  (:documentation
   "Return non-nil if DESCRIPTOR-1 and DESCRIPTOR-2 are compatible for
    the kind of output performed by STYLE. Return nil otherwise."))

(defgeneric incompatible-descriptors (style descriptor-1 descriptor-2)
  (:documentation
   "Signal an error which indicates that DESCRIPTOR-1 and DESCRIPTOR-2
    are not compatible for the kind of output performed by STYLE."))

;;; Temporal bounds protocol

(defgeneric lower-bound (thing)
  (:documentation
   "Return the lower temporal bound of THING. See type `time-spec'."))

(defgeneric (setf lower-bound) (new-value thing)
  (:documentation
   "Set the lower temporal bound of THING to NEW-VALUE. See type
    `time-spec'."))

(defgeneric upper-bound (thing)
  (:documentation
   "Return the upper temporal bound of THING. See type `time-spec'."))

(defgeneric (setf upper-bound) (new-value thing)
  (:documentation
   "Set the upper temporal bound of THING to NEW-VALUE. See type
    `time-spec'."))

(defgeneric bounds (thing)
  (:documentation
   "Return the temporal bounds of THING. See type `bounds-spec'."))

(defgeneric (setf bounds) (new-value thing)
  (:documentation
   "Set the temporal bounds of THING to NEW-VALUE. See type
    `bounds-spec'."))

(defgeneric bounds/expanded (thing &optional now)
  (:documentation
   "Return a list of the form

      (LOWER UPPER)

    containing the temporal bounds of THING as `timestamp/unix/nsec'
    relative to NOW. NOW can be a `local-time:timestamp', a
    `timestamp/unix/nsec' or nil, in which case the current time is
    used."))

(defgeneric range/expanded (thing)
  (:documentation
   "Return the difference between the upper and lower temporal bound
    of THING in nanoseconds."))

;;; Header printing protocol

(defgeneric format-header (thing target)
  (:documentation
   "Format a header for THING into TARGET."))

;;; Column protocol

(defgeneric value< (left right)
  (:documentation
   "Return true if value LEFT should be sorted before value RIGHT."))

(defgeneric column< (left right)
  (:documentation
   "Return true if column LEFT should be sorted before column
    RIGHT."))

(defgeneric column-name (column)
  (:documentation
   "Return the name of COLUMN."))

(defgeneric column-width (column)
  (:documentation
   "Return the width in characters of COLUMN."))

(defgeneric column-produces-output? (column)
  (:documentation
   "Return non-nil if COLUMN produces output when asked to format
    something. This can be nil for example the width of COLUMN has
    been set to zero or if COLUMN is actually a pseudo-column
    producing things like linebreaks."))

;; Default behavior

(defmethod value< ((left real) (right real))
  (< left right))

(defmethod value< ((left string) (right string))
  (string< left right))

(defmethod value< ((left scope) (right scope))
  (value< (scope-string left) (scope-string right)))

(defmethod value< ((left uuid:uuid) (right uuid:uuid))
  (value< (princ-to-string left) (princ-to-string right)))

(defmethod value< ((left t) (right t))
  (let+ (((&flet value? (thing)
            (typep thing '(not (or null (eql :n/a)))))))
    (cond
      ((and (value? left) (not (value? right))))
      ((and (not (value? left)) (value? right))
       nil))))

(defmethod column-produces-output? ((column t))
  "Default implementation assumes that COLUMN produces output if its
   width is positive."
  (plusp (column-width column)))

;;; Column class family

(dynamic-classes:define-findable-class-family column
  "This class family consists of column classes, instances of which
   can be used in column-based formatting styles. Column classes have
   to implement the column protocol consisting of:
   + `column-name'
   + `column-width'
   + [`column-produces-output?' default implementation available]
   + `format-event'")

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
     (let+ (((class &rest args) spec))
       (apply #'make-instance (find-column-class class) args)))
    (standard-object
     spec)))

;;; Dynamic column width protocol

(defgeneric style-dynamic-width-columns (style)
  (:documentation
   "Return a list of the columns of STYLE the width of which should be
    dynamically adjusted."))

(defgeneric style-compute-column-widths (style columns width
                                         &key separator-width)
  (:documentation
   "Compute and return a sequence of widths that distributes the
    available WIDTH among the COLUMNS of STYLE.

    If supplied, SEPARATOR-WIDTH is the width of a separator string
    printed between columns."))

(defgeneric style-assign-column-widths (style columns widths)
  (:documentation
   "Assign the sequence of widths WIDTHS to the COLUMNS of STYLE."))

;; Default behavior

(defmethod style-dynamic-width-columns ((style t))
  (let+ (((&flet width-spec? (column)
            (compute-applicable-methods #'column-widths (list column)))))
    (remove-if-not #'width-spec? (style-columns style))))

(defmethod style-compute-column-widths ((style   t)
                                        (columns sequence)
                                        (width   integer)
                                        &key
                                        (separator-width 0))
  (let ((specs      (map 'list #'column-widths   columns))
        (priorities (map 'list #'column-priority columns)))
    (optimize-widths width specs priorities
                     :separator-width separator-width)))

(defmethod style-assign-column-widths ((style   t)
                                       (columns sequence)
                                       (widths  sequence))
  (map nil (lambda (column width)
             (setf (column-width column) width))
       columns widths))

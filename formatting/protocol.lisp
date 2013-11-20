;;;; protocol.lisp --- Protocol for formatting of RSB events.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
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
MAX-LINES controls specifies the maximum number of lines the produced
output is allowed to take up.
MAX-COLUMNS limits the number of columns individual output lines are
allowed to take up."))

(defgeneric format-payload (data style stream
                            &key
                            max-lines
                            max-columns)
  (:documentation
   "Format the event payload DATA onto STREAM using a formatting style
designated by STYLE.
MAX-LINES controls specifies the maximum number of lines the produced
output is allowed to take up.
MAX-COLUMNS limits the number of columns individual output lines are
allowed to take up."))

;;; Formatting style class family

(dynamic-classes:define-findable-class-family style
    "This class family consists of event formatting style
classes. Each class implements a particular style of formatting
received events onto a given stream by specializing `format-event'.")

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
objects for formatting EVENT. nil indicates that EVENT should not be
processed in any sub-style."))

(defgeneric delegate (event style stream)
  (:documentation
   "Delegate processing of EVENT on STREAM by STYLE to a sub-style."))

(defgeneric make-sub-style-entry (style value)
  (:documentation
   "Create and return a list of the form

  (PREDICATE SUB-STYLE)

where PREDICATE is a function of one parameter, a thing to be
formatted, which decides whether the sub-style SUB-STYLE of STYLE is
suitable for formatting the object. Return nil when no sub-style entry
should be created for VALUE. VALUE depends on how STYLE organizes its
sub-styles. See `sub-style-for' and `delegate'."))

;;; Sub-style sorting protocol

(defgeneric style-sub-styles/sorted (style
                                     &key
                                     predicate
                                     key)
  (:documentation
   "Return a list of style object which are sub-styles of STYLE,
sorted according to PREDICATE and KEY."))

;;; Data consistency protocol

(defgeneric descriptor-for-target (style target)
  (:documentation
   "Return a descriptor of acceptable output send to TARGET by STYLE.

The returned descriptor can be any object returned by a method for
STYLE on `make-descriptor' that works with specialized methods for
STYLE on `compatible-descriptors?' and `incompatible-descriptors'."))

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
STYLE. The descriptor can be derived from DATA when future output has
to be compatible to DATA in some sense.

The returned descriptor has to work with methods on
`compatible-descriptors?' and `incompatible-descriptors' specialized
for STYLE."))

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

(defgeneric bounds/expanded (thing)
  (:documentation
   "Return a list of the form

  (LOWER UPPER)

containing the temporal bounds of THING as `timestamp/unix/nsec'."))

(defgeneric range/expanded (thing)
  (:documentation
   "Return the difference between the upper and lower temporal bound
of THING in nanoseconds."))

;;; Header printing protocol

(defgeneric format-header (thing target)
  (:documentation
   "Format a header for THING into TARGET."))

;;; Column protocol

(defgeneric column-name (column)
  (:documentation
   "Return the name of COLUMN."))

(defgeneric column-width (column)
  (:documentation
   "Return the width in characters of COLUMN."))

(defgeneric column-produces-output? (column)
  (:documentation
   "Return non-nil if COLUMN produces output when asked to format
something. This can be nil for example the width of COLUMN has been
set to zero or if COLUMN is actually a pseudo-column producing things
like linebreaks."))

;;; Default behavior

(defmethod column-produces-output? ((column t))
  "Default implementation assumes that COLUMN produces output if its
width is positive."
  (plusp (column-width column)))

;;; Column class family

(dynamic-classes:define-findable-class-family column
    "This class family consists of column classes, instances of which
can be used in column-based formatting styles. Column classes have to
implement the column protocol consisting of:
+ `column-name'
+ `column-width'
+ [`column-produces-output?' default implementation available]
+ `format-event'")

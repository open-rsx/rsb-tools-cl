;;; protocol.lisp --- Protocol for formatting of RSB events.
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


;;; Event formatting protocol
;;

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
;;

(dynamic-classes:define-findable-class-family style
    "This class family consists of event formatting style
classes. Each class implements a particular style of formatting
received events onto a given stream by specializing `format-event'.")


;;; Delegation protocol
;;

(defgeneric sub-style-for (style event)
  (:documentation
   "Return a sub-style object of STYLE or a sequence of such style
objects for formatting EVENT."))


;;; Column protocol
;;

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
;;

(defmethod column-produces-output? ((column t))
  "Default implementation assumes that COLUMN produces output if its
width is positive."
  (plusp (column-width column)))


;;; Column class family
;;

(dynamic-classes:define-findable-class-family column
    "This class family consists of column classes, instances of which
can be used in column-based formatting styles. Column classes have to
implement the column protocol consisting of:
+ `column-name'
+ `column-width'
+ [`column-produces-output?' default implementation available]
+ `format-event'")

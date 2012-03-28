;;; help.lisp --- Help text generation for formatting options.
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

(defun make-style-help-string (&key
			       (show :default))
  "Return a help string that explains how to specify a formatting
style and its parameters."
  (with-output-to-string (stream)
    (format stream "Specify a formatting style that should be used to ~
print events. SPEC has to be of the form

  KIND KEY1 VALUE1 KEY2 VALUE2 ...

where keys and values are optional and depend on KIND. Examples (note ~
that the single quotes have to be included only when used within a ~
shell):

  --style detailed
  -s compact
  --style 'compact :separator \"|\"'
  --style 'columns :columns (:now (:scope :width 12) :id :newline)'
    \(see extended help, enable with --help-for=columns, for an ~
explanation of the :columns argument\)

")
    (rsb.common:with-abbreviation
	(stream '(:styles :columns :quantities) show)
      (format stream "The following formatting styles are ~
currently available:

")
      (rsb.common:print-classes-help-string
       (style-classes) stream
       :initarg-blacklist '(:stream :pretty-state
			    :quantities :count
			    :sub-styles :test :key
			    :sort-predicate :sort-key))

      (format stream "~%~%")
      (rsb.common:with-abbreviation (stream :columns show)
	(format stream "In column-based formatting styles, columns can ~
be selected and configured using the :columns argument and a syntax of ~
the form

  :columns (COLSPEC1 COLSPEC2 ...)

where

  COLSPEC ::= KIND | (KIND KEY1 VALUE1 KEY2 VALUE2 ...)

The following columns are available:

")
	(rsb.common:print-classes-help-string
	 (column-classes) stream))

      (when-let* ((package (find-package :rsb.stats))
		  (symbol  (find-symbol "QUANTITY-CLASSES" package))
		  (classes (fdefinition symbol)))
	(format stream "~%~%")
	(rsb.common:with-abbreviation (stream :quantities show)
	  (format stream "In the statistics style, statistical ~
quantities are used in columns. These columns can be configured using ~
the :columns argument and a syntax of the form

  :columns (COLSPEC1 COLSPEC2 ...)

where

  COLSPEC      ::= (:quantity :quantity QUANTITYSPEC KEY1 VALUE1 KEY2 ~
VALUE2 ...)
  QUANTITYSPEC ::= KIND | (KIND KEY1 VALUE1 KEY2 VALUE2 ...)

The following quantities are available:

")
	  (rsb.common:print-classes-help-string
	   (funcall classes) stream
	   :initarg-blacklist '(:extractor :reduce-by :start-time :values)))))))

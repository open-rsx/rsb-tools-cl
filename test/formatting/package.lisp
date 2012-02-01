;;; package.lisp --- Package definition for unit tests of the formatting module.
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

(cl:defpackage :rsb.formatting.test
  (:use
   :cl
   :alexandria
   :let-plus
   :lift

   :rsb
   :rsb.formatting)

  (:export
   :formatting-root)

  (:documentation
   "This package contains unit tests for the formatting module."))

(cl:in-package :rsb.formatting.test)

(defvar *simple-event* (make-event "/foo" "bar")
  "A simple event for use in tests.")

(defvar *simple-events* (list *simple-event* (make-event "/baz" "fez"))
  "A sequence of simple events for use in tests.")

(deftestsuite formatting-root ()
  ()
  (:documentation
   "Root unit test suite for the formatting module."))

(defmacro ensure-style-cases ((class) &body cases)
  "Generate tests cases for methods on `format-event' for CLASS
according to CASES. Each element of cases has to be of the form

  (INITARGS DATA EXPECTED-OUTPUT)

For each case, an instance of CLASS is constructed with INITARGS,
`format-event' is applied to each element of DATA and the resulting
output is compared to EXPECTED-OUTPUT."
  `(ensure-cases (args data expected)
       (list ,@cases)

     (let+ ((instance (apply #'make-instance ',class args))
	    ((&flet do-it ()
	       (with-output-to-string (stream)
		 (map nil (rcurry #'format-event instance stream)
		      data)))))
       (if (eq expected :error)
	   (ensure-condition error (do-it))
	   (let ((output (do-it)))
	     (ensure-same (concatenate 'string "^" expected "$") output
			  :test      #'ppcre:scan
			  :report    "~@<The formatting style ~A, when ~
applied to ~:[no data~:;the data ~:*~{~S~^, ~}~], produced the output ~
~S, not ~S.~@:>"
			  :arguments (instance data output expected)))))))

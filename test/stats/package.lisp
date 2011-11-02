;;; package.lisp --- Package definition for unit tests of the stats module.
;;
;; Copyright (C) 2011 Jan Moringen
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

(cl:defpackage :rsb.stats.test
  (:use
   :cl
   :alexandria
   :bind
   :lift

   :rsb
   :rsb.stats)

  (:export
   :stats-root)

  (:documentation
   "This package contains unit tests for the stats module"))

(cl:in-package :rsb.stats.test)

(deftestsuite stats-root ()
  ()
  (:documentation
   "Root unit test suite for the stats module."))

(defmacro ensure-quantity-cases ((class) &body cases)
  "Generate tests cases for methods on `update!' and `format-value'
for quantity CLASS according to CASES. Each element of cases has to be
of the form

  (INITARGS DATA EXPECTED-OUTPUT)

For each case, an instance of CLASS is constructed with INITARGS, DATA
is processed via calls to `update!' and output is produced by finally
calling `format-value'. The resulting output is compared to
EXPECTED-OUTPUT."
  `(ensure-cases (args events expected)
       (progn ,@cases)

     (if (eq expected :error)
	 (ensure-condition 'error
	   (apply #'make-instance ',class args))
	 (let* ((instance (apply #'make-instance ',class args))
		(output   (progn
			    (reset! instance)
			    (map nil (curry #'update! instance) events)
			    (with-output-to-string (stream)
			      (format-value instance stream)))))
	   (ensure-same expected output
			:test      #'ppcre:scan
			:report    "~@<The quantity ~A, when updated with ~
data ~{~S~^, ~}, produced the output ~S, not ~S.~@:>"
			:arguments (instance events output expected))))))

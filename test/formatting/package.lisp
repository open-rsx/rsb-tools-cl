;;;; package.lisp --- Package definition for unit tests of the formatting module.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb.formatting.test
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:more-conditions
   #:lift

   #:rsb
   #:rsb.formatting)

  (:export
   #:formatting-root)

  (:documentation
   "This package contains unit tests for the formatting module."))

(cl:in-package #:rsb.formatting.test)

(defvar *simple-event* (make-event "/foo" "bar")
  "A simple event for use in tests.")

(defvar *simple-events* (list *simple-event* (make-event "/baz" "fez"))
  "A sequence of simple events for use in tests.")

(deftestsuite formatting-root ()
  ()
  (:timeout 20)
  (:documentation
   "Root unit test suite for the formatting module."))

(defmacro ensure-style-cases ((class &key (formatter 'format-event))
                              &body cases)
  "Generate tests cases for methods on `format-event' for CLASS
   according to CASES. Each element of cases has to be of the form

     (INITARGS DATA EXPECTED-OUTPUT)

   For each case, an instance of CLASS is constructed with INITARGS,
   `format-event' is applied to each element of DATA and the resulting
   output is compared to EXPECTED-OUTPUT."
  (let ((formatter (case formatter
                     (:format-header '(lambda (event style stream)
                                       (declare (ignore event))
                                       (format-header style stream)))
                     (t              formatter))))
    `(ensure-cases (args data expected)
         (list ,@cases)

       (let+ ((instance (apply #'make-instance ',class args))
              ((&flet do-it ()
                 (with-output-to-string (stream)
                   (map nil (lambda (data)
                              (rsb.test:with-access-checking ()
                                (,formatter
                                 (maybe-add-access-checking data instance)
                                 instance
                                 stream)))
                        data)))))
         (case expected
           (:error
            (ensure-condition error (do-it)))
           (t
            (let ((output (do-it)))
              (ensure-same (concatenate 'string "^" expected "$") output
                           :test      #'ppcre:scan
                           :report    "~@<The formatting style ~A, when ~
                                       applied to ~:[no data~:;the data ~
                                       ~:*~{~S~^, ~}~], produced the output ~
                                       ~S, not ~S.~@:>"
                           :arguments (instance data output expected)))))))))

(defun maybe-add-access-checking (data processor)
  (typecase data
    (event (rsb.test:add-access-checking-to-event
            data (rsb.test:access-rules-for-processor processor)))
    (t     data)))

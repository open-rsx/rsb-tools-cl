;;;; package.lisp --- Package definition for unit tests of the stats module.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb.stats.test
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:lift

   #:rsb
   #:rsb.stats)

  (:export
   #:stats-root)

  (:documentation
   "This package contains unit tests for the stats module"))

(cl:in-package #:rsb.stats.test)

(deftestsuite stats-root ()
  ()
  (:documentation
   "Root unit test suite for the stats module."))

(defmacro ensure-quantity-cases ((class &key (reset? t)) &body cases)
  "Generate tests cases for methods on `update!' and `format-value'
   for quantity CLASS according to CASES. Each element of cases has to
   be of the form

     (INITARGS DATA EXPECTED-OUTPUT)

   For each case, an instance of CLASS is constructed with INITARGS,
   DATA is processed via calls to `update!' and output is produced by
   finally calling `format-value'. The resulting output is compared to
   EXPECTED-OUTPUT."
  `(ensure-cases (args events expected)
       (progn ,@cases)

     (case expected
       (error
        (ensure-condition 'error
          (apply #'make-instance ',class args)))
       (t
        (let+ ((instance (apply #'make-instance ',class args))
               ((&flet do-it (&optional reset?)
                  (reset! instance)
                  (let ((output (progn
                                  ;; Wait 1 ms for the sake of
                                  ;; time-based quantities.
                                  (sleep .001)
                                  (mapc (curry #'update! instance) events)
                                  (with-output-to-string (stream)
                                    (format-value instance stream)))))
                    (ensure-same
                     expected output
                     :test      #'ppcre:scan
                     :report    "~@<The quantity ~A~:[~:;, after being ~
                                 reset~], when ~:[not updated with ~
                                 any data~;~:*updated with data ~
                                 ~{~S~^, ~}~], produced the output ~S, ~
                                 not ~S.~@:>"
                     :arguments (instance reset? events output expected))))))
          (do-it)
          ,@(when reset? `((do-it t))))))))

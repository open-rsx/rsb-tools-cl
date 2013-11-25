;;;; quantities.lisp --- Unit tests for quantity classes.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.stats.test)

(defmacro define-simple-quantity-suite ((name &key (reset? t)) &body cases)
  "Define a test suite for the quantity class designated by NAME. Use
   CASES as body of `ensure-quantity-cases' in a test case for the
   `update!' and `format-value' methods."
  (let ((class-name (class-name (find-quantity-class name)))
        (suite-name (symbolicate name "-ROOT")))
    `(progn
       (deftestsuite ,suite-name (stats-root)
         ()
         (:documentation
          ,(format nil "Test suite for the `~(~A~)' quantity class."
                   class-name)))

       (addtest (,suite-name
                 :documentation
                 ,(format nil "Test methods on `update!' and ~
                               `format-value' for the `~(~A~)' ~
                               quantity class."
                          class-name))
         update!-and-format-value

         (let+ (((&flet event (&optional (scope "/foo") (data "bar"))
                   (make-event scope data))))
           (ensure-quantity-cases (,class-name :reset? ,reset?)
             (progn ,@cases)))))))

(define-simple-quantity-suite (:count)
  `((() ()         "^0$")
    (() (,(event)) "^1$")
    (() (,(event)) "^1$")))

(define-simple-quantity-suite (:count/all-time :reset? nil)
  `((() ()         "^0$")
    (() (,(event)) "^1$")
    (() (,(event)) "^1$")))

(define-simple-quantity-suite (:rate)
  `((() ()         "^0\\.000$")
    (() (,(event)) "^[0-9]+\\.[0-9]+$")))

(define-simple-quantity-suite (:period-time)
  `((() ()                        "^N/A ± N/A$")
    (() (,(event))                "^N/A ± N/A$")
    (() (,(event) ,(progn
                     (sleep .001)
                     (event)))    "^0\\.[0-9][0-9][0-9] ± [0-9]\\.[0-9][0-9][0-9]$")))

(define-simple-quantity-suite (:throughput)
  `((() ()         "^0\\.000$")
    (() (,(event)) "^[0-9]+\\.[0-9]+$")))

(define-simple-quantity-suite (:size)
    `((() ()                      "^N/A ± N/A")
      (() (,(event "/foo" "bar")) "^3\\.000 ± 0\\.000$")))

(define-simple-quantity-suite (:size/log)
    `((() ()                      "^N/A$")
      (() (,(event "/foo" "bar")) "^4: 1$")))

(define-simple-quantity-suite (:size/all-time :reset? nil)
    `((() ()                      "^0$")
      (() (,(event "/foo" "bar")) "^3$")))

(define-simple-quantity-suite (:scope)
  `((() ()                                                "^N/A$")
    (() (,(event "/foo"))                                 "^/foo/: 1$")
    (() (,(event "/foo") ,(event "/bar") ,(event "/bar")) "^/bar/: 2, /foo/: 1$")))

(define-simple-quantity-suite (:method)
  (let+ (((&flet event (method)
            (make-event "/foo" "baz" :method method))))
    `((() ()                                    "^N/A$")
      (() (,(event :a))                         "^A: 1$")
      (() (,(event :a) ,(event :b) ,(event :b)) "^B: 2, A: 1$"))))

(define-simple-quantity-suite (:origin)
  (let+ ((id1 (uuid:make-v4-uuid))
         (id2 (uuid:make-v4-uuid))
         ((&flet from (origin)
            (let ((event (make-event "/foo" "baz")))
              (setf (event-origin event) origin)
              event))))
    `((() ()                                    "^N/A$")
      (() (,(from id1))                         ,(format nil "^~A: 1$" id1))
      (() (,(from id1) ,(from id2) ,(from id2)) ,(format nil "^~A: 2, ~A: 1$"
                                                         id2 id1)))))

(define-simple-quantity-suite (:wire-schema)
  (let+ (((&flet event (wire-schema)
            (make-event "/foo" "baz" :rsb.transport.wire-schema wire-schema))))
    `((() ()                                        "^N/A$")
      (() (,(event "a"))                            "^a: 1$")
      (() (,(event "a") ,(event "b") ,(event  "b")) "^b: 2, a: 1$"))))

(define-simple-quantity-suite (:type)
    `((() ()                       "^N/A$")
      (() (,(make-event "/foo" 1)) "^BIT: 1$")))

(define-simple-quantity-suite (:meta-data-moments)
  (let+ (((&flet event (&rest args &key &allow-other-keys)
            (apply #'make-event "/foo" "baz" args))))
    `(;; missing required initarg :key
      (()             ()                                    error)
      ;; cannot compute moment on meta-data keys
      ((:key :keys)   ()                                    error)
      ;; some key
      ((:key :foo)    ()                                    "^N/A ± N/A$")
      ((:key :foo)    (,(event :foo "1"))                   "^1\\.000 ± 0\\.000$")
      ((:key :foo)    (,(event :foo "1") ,(event :bar "2")
                       ,(event :foo "3") ,(event :foo "4")) "^2\\.667 ± 1\\.247$")
      ;; values
      ((:key :values) ()                                    "^N/A ± N/A$")
      ((:key :values) (,(event :foo "1"))                   "^1\\.000 ± 0\\.000$")
      ((:key :values) (,(event :foo "1") ,(event :bar "2")
                       ,(event :foo "3") ,(event :foo "4")) "^2\\.500 ± 1\\.118$"))))

(define-simple-quantity-suite (:latency)
  (let+ (((&flet args (&rest keys)
            `(,@(when-let ((value (first keys)))
                  `(:from ,value))
              ,@(when-let ((value (second keys)))
                  `(:to ,value))))))
    `(;; missing :from and :to initargs
      (()                      ()                           error)
      (,(args :create)         ()                           error)
      (,(args nil :create)     ()                           error)
      ;; valid cases
      (,(args :create :create) ()                           "^N/A ± N/A$")
      (,(args :create :create) (,(make-event "/foo" "baz")) "^[0-9]\\.[0-9]{3} ± [0-9]\\.[0-9]{3}$"))))

(define-simple-quantity-suite (:expected)
  `(;; missing :target and :expected initargs
    (()                                         ()         error)
    ((:target :does-not-matter)                 ()         error)
    ((:expected :does-not-matter)               ()         error)
    ;; valid cases
    ((:target :count :expected 0)               ()         "^0$")
    ((:target :count :expected 1)               ()         "^!1: 0$")
    ((:target :count :expected 1)               (,(event)) "^1$")
    ((:target :count :expected (:type (eql 0))) ()         "^0$")
    ((:target :count :expected (:type (eql 1))) ()         "^not a \\(EQL 1\\): 0$")
    ((:target :count :expected (:type (eql 1))) (,(event)) "^1$")))

;; Local Variables:
;; coding: utf-8
;; End:

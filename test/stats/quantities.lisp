;;; quantities.lisp --- Unit tests for quantity classes.
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

(in-package :rsb.stats.test)

(defmacro define-simple-quantity-suite ((name) &body cases)
  ""
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
`format-value' for the `~(~A~)' quantity class."
			  class-name))
	 update!-and-format-value

	 (ensure-quantity-cases (,class-name)
	   (progn ,@cases))))))

(define-simple-quantity-suite (:rate)
  `((() ()                           "^N/A$")
    (() (,(make-event "/foo" "bar")) "^[0-9]+\\.[0-9]+$")))

(define-simple-quantity-suite (:throughput)
  `((() ()                           "^0\\.000$")
    (() (,(make-event "/foo" "bar")) "^[0-9]+\\.[0-9]+$")))

(define-simple-quantity-suite (:scope)
  `((() ()                           "^N/A$")
    (() (,(make-event "/foo" "baz")) "^/foo/: 1$")
    (() (,(make-event "/foo" "baz")
	 ,(make-event "/bar" "baz")
	 ,(make-event "/bar" "baz")) "^/bar/: 2, /foo/: 1$")))

(define-simple-quantity-suite (:method)
  `((() ()                           "^N/A$")
    (() (,(make-event "/foo" "baz"
		      :method :a)) "^A: 1$")
    (() (,(make-event "/foo" "baz"
		      :method :a)
	 ,(make-event "/bar" "baz"
		      :method :b)
	 ,(make-event "/bar" "baz"
		      :method :b)) "^B: 2, A: 1$")))

(define-simple-quantity-suite (:origin)
  (bind ((id1 (uuid:make-v4-uuid))
	 (id2 (uuid:make-v4-uuid))
	 ((:flet from (origin))
	  (let ((event (make-event "/foo" "baz")))
	    (setf (event-origin event) origin)
	    event)))
    `((() ()                                    "^N/A$")
      (() (,(from id1))                         ,(format nil "^~A: 1$" id1))
      (() (,(from id1) ,(from id2) ,(from id2)) ,(format nil "^~A: 2, ~A: 1$"
							 id2 id1)))))

(define-simple-quantity-suite (:wire-schema)
  `((() ()                                   "^N/A$")
    (() (,(make-event "/foo" "baz"
		      :rsb.wire-schema "a")) "^a: 1$")
    (() (,(make-event "/foo" "baz"
		      :rsb.wire-schema "a")
	 ,(make-event "/bar" "baz"
		      :rsb.wire-schema "b")
	 ,(make-event "/bar" "baz"
		      :rsb.wire-schema "b")) "^b: 2, a: 1$")))

(define-simple-quantity-suite (:meta-data-moments)
  (bind (((:flet event (&rest args &key &allow-other-keys))
	  (apply #'make-event "/foo" "baz" args)))
    `(;; missing required initarg :key
      (()             ()                                    :error)
      ;; cannot compute moment on meta-data keys
      ((:key :keys)   ()                                    :error)
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
  (bind (((:flet args (&rest keys))
	  `(,@(when-let ((value (first keys)))
	        `(:from ,value))
	    ,@(when-let ((value (second keys)))
	        `(:to ,value)))))
    `(;; missing :from and :to initargs
      (()                      ()                           :error)
      (,(args :create)         ()                           :error)
      (,(args nil :create)     ()                           :error)
      ;; valid cases
      (,(args :create :create) ()                           "^N/A ± N/A$")
      (,(args :create :create) (,(make-event "/foo" "baz")) "^[0-9]\\.[0-9]{3} ± [0-9]\\.[0-9]{3}$"))))

;; Local Variables:
;; coding: utf-8
;; End:

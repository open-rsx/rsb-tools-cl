;;;; protocol-buffer-payload.lisp --- Unit tests for protocol buffer payloads.
;;;;
;;;; Copyright (C) 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.common.test)

(deftestsuite protocol-buffer-payload-root (common-root)
  ()
  (:documentation
   "Test suite for error-handling functions."))

(addtest (protocol-buffer-payload-root
          :documentation
          "Smoke test for parsing protocol buffer messages.")
         grammar/smoke

  (ensure-cases (input expected)
      `(;; Some invalid cases.
        (""       error)
        ("{"      error)
        ("<"      error)
        ("["      error)
        ("foo"    error)
        ("{foo}"  error)
        ("{foo:}" error)
        ;; These are OK.
        ("{}"
         (:message ()))
        ("<>"
         (:message ()))
        ("{foo:1}"
         (:message (:field ((:field (:value ((:literal () :value 1)))
                                    :name "foo")))))
        ("{foo:1.0}"
         (:message (:field ((:field (:value ((:literal () :value 1.0f0)))
                                    :name "foo")))))
        ("{foo:\"foo\\000\"}"
         (:message (:field ((:field (:value ((:literal () :value ,(format nil "foo~C" #\Nul))))
                                    :name "foo")))))
        ("<foo:[1,2]>"
         (:message (:field ((:field (:value ((:literal () :value 1)
                                             (:literal () :value 2)))
                                    :name "foo")))))
        ;; Negative float literals in ]-1,0[ were broken at one point.
        ("{foo:-1}"
         (:message (:field ((:field (:value ((:literal () :value -1)))
                                    :name "foo")))))
        ("{foo:+.5}"
         (:message (:field ((:field (:value ((:literal () :value 0.5f0)))
                                    :name "foo")))))
        ("{foo:-.5}"
         (:message (:field ((:field (:value ((:literal () :value -0.5f0)))
                                    :name "foo")))))
        ("{foo:.5}"
         (:message (:field ((:field (:value ((:literal () :value 0.5f0)))
                                    :name "foo")))))
        ("{foo:+0.5}"
         (:message (:field ((:field (:value ((:literal () :value 0.5f0)))
                                    :name "foo")))))
        ("{foo:-0.5}"
         (:message (:field ((:field (:value ((:literal () :value -0.5f0)))
                                    :name "foo")))))
        ("{foo:0.5}"
         (:message (:field ((:field (:value ((:literal () :value 0.5f0)))
                                    :name "foo"))))))

    (let+ (((&flet do-it ()
              (let ((architecture.builder-protocol:*builder* 'list))
                (esrap:parse 'rsb.common::message input)))))
      (case expected
        (error (ensure-condition 'error (do-it)))
        (t     (ensure-same (do-it) expected :test #'equal))))))

(addtest (protocol-buffer-payload-root
          :documentation
          "Smoke test for building protocol protocol buffer
           messages.")
  build/smoke

  (ensure-cases (input &optional (expected t))
      '(;; Some invalid cases.
        ("{no_such_field:1}"          error)
        ("{scope: {}}"                error)
        ("{scope: [\"foo\",\"bar\"]}" error)
        ;; These are OK.
        ("{}")
        ("{meta_data: {}}")
        ("{data: \"\\000\\000\"}"))

    (let+ (((&flet do-it ()
              (rsb.common::build-protocol-buffer-message
               (pb:find-descriptor ".rsb.protocol.Notification") input))))
      (case expected
        (error (ensure-condition 'error (do-it)))
        (t     (do-it))))))

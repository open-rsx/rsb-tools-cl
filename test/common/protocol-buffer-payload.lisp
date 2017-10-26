;;;; protocol-buffer-payload.lisp --- Unit tests for protocol buffer payloads.
;;;;
;;;; Copyright (C) 2015, 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.tools.common.test)

(deftestsuite protocol-buffer-payload-root (common-root)
  ()
  (:documentation
   "Test suite for error-handling functions."))

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
              (rsb.tools.common::build-protocol-buffer-message
               (pb:find-descriptor ".rsb.protocol.Notification") input))))
      (case expected
        (error (ensure-condition 'error (do-it)))
        (t     (do-it))))))

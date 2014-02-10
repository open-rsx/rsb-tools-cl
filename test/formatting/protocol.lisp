;;;; protocol.lisp --- Unit tests for the protocol of the formatting module.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.formatting.test)

(deftestsuite formatting-protocol-root (formatting-root)
  ()
  (:documentation
   "Test suite for the protocol of the formatting module."))

(addtest (formatting-protocol-root
          :documentation
          "Smoke test for the `make-style' generic function.")
  make-style/smoke

  (ensure-cases (args expected)
      `(;; Non-existent style.
        ((:no-such-style)                          style-creation-error)
        (((:no-such-style))                        style-creation-error)
        (((:no-such-style :baz 2))                 style-creation-error)
        ((:no-such-style :bar 1)                   style-creation-error)
        (((:no-such-style) :bar 1)                 style-creation-error)
        (((:no-such-style :baz 2) :bar 1)          style-creation-error)
        ;; Cannot add initargs to already constructed instance.
        ((,(make-style :compact) :foo 1)           style-creation-error)

        ;; These are OK.
        ((:compact)                                t)
        (((:compact))                              t)
        (((:compact :header-frequency 2))          t)
        ((:compact :header-frequency 2)            t)
        (((:compact) :header-frequency 2)          t)
        (((:compact :count 0) :header-frequency 2) t))

    (let+ (((&flet do-it () (apply #'make-style args))))
      (ecase expected
        (style-creation-error
         (ensure-condition style-creation-error (do-it)))
        ((t)
         (do-it))))))

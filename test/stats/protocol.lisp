;;;; protocol.lisp --- Unit tests for the protocol of the stats module.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.stats.test)

(deftestsuite formatting-protocol-root (stats-root)
  ()
  (:documentation
   "Test suite for the protocol of the stats module."))

(addtest (formatting-protocol-root
          :documentation
          "Smoke test for the `make-quantity' generic function.")
           make-quantity/smoke

  (ensure-cases (args expected)
      `(;; Non-existent quantity.
        ((:no-such-quantity)                 quantity-creation-error)
        (((:no-such-quantity))               quantity-creation-error)
        (((:no-such-quantity :baz 2))        quantity-creation-error)
        ((:no-such-quantity :bar 1)          quantity-creation-error)
        (((:no-such-quantity) :bar 1)        quantity-creation-error)
        (((:no-such-quantity :baz 2) :bar 1) quantity-creation-error)
        ;; Cannot add initargs to already constructed instance.
        ((,(make-quantity :rate) :foo 1)     quantity-creation-error)

        ;; These are OK.
        ((:rate)                             t)
        (((:rate))                           t)
        (((:rate :format "~,5F"))            t)
        ((:rate :format "~,5F")              t)
        (((:rate) :format "~,5F")            t)
        (((:rate :name "") :format "~,5F")   t))

    (let+ (((&flet do-it () (apply #'make-quantity args))))
      (ecase expected
        (quantity-creation-error
         (ensure-condition quantity-creation-error (do-it)))
        ((t)
         (do-it))))))

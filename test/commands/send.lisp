;;;; send.lisp --- Tests for the send command class.
;;;;
;;;; Copyright (C) 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.tools.commands.test)

(deftestsuite send-root (commands-root)
  ()
  (:documentation
   "Test suite for the `send' command."))

(addtest (send-root
          :documentation
          "Test construction of the `send' command.")
  construction

  (ensure-cases (initargs &optional expected)
      `(;; Some invalid cases with missing initargs.
        (()                 missing-required-initarg) ; :destination is missing
        ((:destination "/") missing-required-initarg) ; :payload[-spec] is missing

        ;; These are Ok.
        ((:destination ,(puri:uri "/") :payload nil))
        ((:destination ,(rsb:make-scope "/") :payload nil))
        ((:destination "/" :payload nil))
        ((:destination ,(rsb:make-scope "/") :payload-spec "true")))

    (let+ (((&flet do-it () (apply #'make-command :send initargs))))
      (case expected
        (missing-required-initarg
         (ensure-condition missing-required-initarg (do-it)))
        (t
         (ensure (typep (princ-to-string (do-it)) 'string)))))))

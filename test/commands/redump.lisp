;;;; redump.lisp --- Tests for the redump command class.
;;;;
;;;; Copyright (C) 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.tools.commands.test)

(deftestsuite redump-root (commands-root)
  ()
  (:documentation
   "Test suite for the `redump' command."))

(addtest (redump-root
          :documentation
          "Test construction of the `redump' command.")
  construction

  (ensure-cases (initargs &optional expected)
      `(;; Some invalid cases.
        (()                              missing-required-initarg) ; :output-file is missing

        ;; These are Ok.
        ((:output-file ,#P"foo"))
        ((:output-file ,#P"foo" :static? t))
        ((:output-file ,#P"foo" :compression 8)))

    (let+ (((&flet do-it () (apply #'make-command :redump initargs))))
      (case expected
        (missing-required-initarg
         (ensure-condition missing-required-initarg (do-it)))
        (t
         (ensure (typep (princ-to-string (do-it)) 'string)))))))

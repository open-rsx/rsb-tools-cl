;;;; web.lisp --- Tests for the web command class.
;;;;
;;;; Copyright (C) 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.tools.commands.test)

(deftestsuite web-root (commands-root)
  ()
  (:documentation
   "Test suite for the `web' command."))

(addtest (web-root
          :documentation
          "Test construction of the `web' command.")
  construction

  (ensure-cases (initargs &optional expected)
      `(;; Some invalid cases.
        (() missing-required-initarg) ; :uris is missing

        ;; These are Ok.
        ((:uris             (,(puri:uri "/"))))
        ((:uris             (,(puri:uri "/"))
          :address          nil))
        ((:uris             (,(puri:uri "/"))
          :address          "0.0.0.0"))
        ((:uris             (,(puri:uri "/"))
          :port             4567))
        ((:uris             (,(puri:uri "/"))
          :document-root    #"static/"))
        ((:uris             (,(puri:uri "/"))
          :document-root    nil))
        ((:uris             (,(puri:uri "/"))
          :response-timeout 1.0)))

    (let+ (((&flet do-it () (apply #'make-command :web initargs))))
      (case expected
        (missing-required-initarg
         (ensure-condition missing-required-initarg (do-it)))
        (t
         (ensure (typep (princ-to-string (do-it)) 'string)))))))

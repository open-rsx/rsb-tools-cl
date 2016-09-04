;;;; command.lisp --- Tests for the web command class.
;;;;
;;;; Copyright (C) 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.tools.commands.web.test)

(deftestsuite commands-web-command-root (commands-web-root)
  ()
  (:documentation
   "Test suite for the `web' command."))

(addtest (commands-web-command-root
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

(addtest (commands-web-command-root
          :documentation
          "Smoke test for the `web' command.")
  smoke

  (let ((configuration *introspection-configuration*)
        (command       (make-command :web :uris '("/") :port 0)))
    (with-asynchronously-executing-command
        (command :bindings ((rsb:*configuration* configuration)))
      (sleep 1) ; TODO racy
      (let ((rsb:*configuration* configuration))
        (rsb:with-participant
            (nil :listener "/rsbtest/tools/commands/web/listener"))))))

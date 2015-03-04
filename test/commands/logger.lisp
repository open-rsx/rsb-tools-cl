;;;; logger.lisp --- Tests for the logger command class.
;;;;
;;;; Copyright (C) 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.tools.commands.test)

(deftestsuite logger-root (commands-root)
  ()
  (:documentation
   "Test suite for the `logger' command."))

(addtest (logger-root
          :documentation
          "Test construction of the `logger' command.")
  construction

  (ensure-cases (initargs &optional expected)
      `(;; Some invalid cases.
        (()                              missing-required-initarg) ; :uris is missing
        ((:uris       (,(puri:uri "/"))) missing-required-initarg) ; :style or :style-spec is missing
        ((:style-spec "detailed")        missing-required-initarg) ; :uris is missing
        ;; These are Ok.
        ((:uris              (,(puri:uri "/"))
          :style-spec        "detailed"))
        ((:uris              (,(rsb:make-scope "/"))
          :style-spec        "detailed"))
        ((:uris              ("/")
          :style-spec        "detailed"))
        ((:uris              (,(puri:uri "/"))
          :style             ,(rsb.formatting:make-style :detailed)))
        ((:uris              (,(puri:uri "/"))
          :style-spec        "detailed"
          :max-queued-events 100))
        ((:uris              (,(puri:uri "/"))
          :style-spec        "detailed"
          :filters           (,(rsb.filter:filter :scope :scope "/"))))
        ((:uris              (,(puri:uri "/"))
          :style-spec        "detailed"
          :stream            ,*error-output*)))

    (let+ (((&flet do-it () (apply #'make-command :logger initargs))))
      (case expected
        (missing-required-initarg
         (ensure-condition missing-required-initarg (do-it)))
        (t
         (ensure (typep (princ-to-string (do-it)) 'string)))))))

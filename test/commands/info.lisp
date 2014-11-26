;;;; info.lisp --- Tests for the info command class.
;;;;
;;;; Copyright (C) 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.tools.commands.test)

(deftestsuite info-root (commands-root)
  ()
  (:documentation
   "Test suite for the `info' command."))

(addtest (info-root
          :documentation
          "Test construction of the `info' command.")
  construction

  (ensure-cases (initargs &optional expected)
      `(;; These are Ok.
        ((:version?          t))
        ((:configuration?    t))
        ((:connectors?       t))
        ((:converters?       t))
        ((:filters?          t))
        ((:transforms?       t))
        ((:event-processing? t))
        ((:participants?     t))
        ((:stream            ,*standard-output*))
        ((:stream-spec       :error-output)))

    (let+ (((&flet do-it () (apply #'make-command :info initargs))))
      (ensure (typep (princ-to-string (do-it)) 'string)))))

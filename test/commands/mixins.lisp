;;;; mixins.lisp --- Tests for command mixin classes.
;;;;
;;;; Copyright (C) 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.tools.commands.test)

(deftestsuite output-stream-mixin-root (commands-root)
  ()
  (:documentation
   "Test suite for the `output-stream-mixin' command mixin class."))

(addtest (output-stream-mixin-root
          :documentation
          "Test construction of the `output-stream-mixin' command.")
  construction

  (ensure-cases (initargs &optional expected)
      `(;; Some invalid cases with incompatible initargs.
        ((:stream      ,*standard-output*
          :stream-spec :error-output)
         incompatible-initargs)

        ;; These are Ok.
        ((:stream      ,*standard-output*))
        ((:stream-spec :error-output)))

    (let+ (((&flet do-it ()
              (apply #'make-instance 'output-stream-mixin initargs))))
      (case expected
        (missing-required-initarg
         (ensure-condition missing-required-initarg (do-it)))
        (incompatible-initargs
         (ensure-condition incompatible-initargs (do-it)))
        (t
         (ensure (typep (princ-to-string (do-it)) 'string)))))))

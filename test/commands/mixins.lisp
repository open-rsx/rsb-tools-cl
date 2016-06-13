;;;; mixins.lisp --- Tests for command mixin classes.
;;;;
;;;; Copyright (C) 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.tools.commands.test)

(deftestsuite style-mixin-root (commands-root)
  ()
  (:documentation
   "Test suite for the `style-mixin' command mixin class."))

(addtest (style-mixin-root
          :documentation
          "Test construction of the `style-mixin' command mixin
           class.")
  construction

  (ensure-cases (initargs &optional expected)
      `(;; Invalid case with missing required initargs.
        (()                                           missing-required-initarg)
        ;; Some invalid cases with incompatible initargs.
        ((:style :a :style-spec :b)                   incompatible-initargs)
        ((:style :a :style-service :b)                incompatible-initargs)
        ((:style :a :style-spec :b :style-service :c) incompatible-initargs)
        ;; This is OK.
        ((:style :a)))
    (let+ (((&flet do-it ()
              (apply #'make-instance 'style-mixin initargs))))
      (case expected
        (missing-required-initarg
         (ensure-condition missing-required-initarg (do-it)))
        (incompatible-initargs
         (ensure-condition incompatible-initargs (do-it)))
        (t
         (ensure (typep (princ-to-string (do-it)) 'string)))))))

(deftestsuite output-stream-mixin-root (commands-root)
  ()
  (:documentation
   "Test suite for the `output-stream-mixin' command mixin class."))

(addtest (output-stream-mixin-root
          :documentation
          "Test construction of the `output-stream-mixin' command
           mixin class.")
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

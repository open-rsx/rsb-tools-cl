;;;; call.lisp --- Tests for the call command class.
;;;;
;;;; Copyright (C) 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.tools.commands.test)

(deftestsuite call-root (commands-root)
  ()
  (:documentation
   "Test suite for the `call' command."))

(addtest (call-root
          :documentation
          "Test construction of the `call' command.")
  construction

  (ensure-cases (initargs &optional expected)
      `(;; Some invalid cases with missing initargs.
        (()                                    missing-required-initarg) ; :destination is missing
        ((:destination "/")                    missing-required-initarg) ; :method is missing
        ((:destination "/"
          :method      "foo")
                                               missing-required-initarg) ; :payload[-spec] is missing
        ((:destination "/"
          :method      "foo"
          :payload     nil)
                                               missing-required-initarg) ; :style[-spec] is missing

        ;; Some invalid cases with incompatible initargs.
        ((:destination "/"
          :call-spec   "socket:/foo/bar(true)"
          :style-spec  "detailed")
                                               incompatible-initargs)
        ((:destination "/"
          :method      "foo"
          :payload     nil
          :style-spec  "detailed"
          :timeout     1
          :no-wait?    t)
                                               incompatible-initargs)

        ;; These are Ok.
        ((:destination ,(puri:uri "/")
          :method      "foo"
          :payload     nil
          :style-spec  "detailed"))
        ((:destination ,(rsb:make-scope "/")
          :method      "foo"
          :payload     nil
          :style-spec  "detailed"))
        ((:destination "/"
          :method      "foo"
          :payload     nil
          :style-spec  "detailed"))
        ((:call-spec   "socket:/foo/bar(true)"
          :style-spec  "detailed"))
        ((:call-spec   "socket:/foo/bar(true)"
          :style-spec  "detailed"
          :timeout     1))
        ((:call-spec   "socket:/foo/bar(true)"
          :style-spec  "detailed"
          :no-wait?    t)))

    (let+ (((&flet do-it () (apply #'make-command :call initargs))))
      (case expected
        (missing-required-initarg
         (ensure-condition missing-required-initarg (do-it)))
        (incompatible-initargs
         (ensure-condition incompatible-initargs (do-it)))
        (t
         (ensure (typep (princ-to-string (do-it)) 'string)))))))

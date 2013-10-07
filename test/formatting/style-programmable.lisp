;;;; style-programmable.lisp --- Unit tests for the programmable formatting style.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsb.formatting.test)

;;; Test suite for `style-programmable' class

(deftestsuite style-programmable-root (formatting-root)
  ()
  (:documentation
   "Unit tests for the `style-programmable' formatting style class."))

(addtest (style-programmable-root
          :documentation
          "Test constructing `style-programmable' instances.")
  construct

  (ensure-cases (args expected)
      '(;; Missing required initargs.
        (()                           error)
        ((:bindings nil)              error)

        ;; Invalid initarg types.
        ((:bindings 5)                error)

        ;; Invalid initarg values.
        ((:code (no-such-variable))   error)
        ((:code (data) :bindings nil) error)

        ;; Produces a warning.
        ((:code ((no-such-function))) ((no-such-function)))

        ;; These are OK.
        ((:code ())                   ())
        ((:code (data))               (data))
        ((:code (skip-event))         (skip-event)))

    (if (eq expected 'error)
        (ensure-condition error
          (apply #'make-instance 'style-programmable args))
        (let ((style (apply #'make-instance 'style-programmable
                            args)))
          (ensure-same (style-code style) expected :test #'equal)))))

(addtest (style-programmable-root
          :documentation
          "Test some simple cases of formatting events using methods
on `format-event' for `style-programmable'.")
  smoke

  (ensure-style-cases (style-programmable)
    ;; These are OK.
    '((:code nil)                   ()                "")
    `((:code nil)                   (,*simple-event*) "")
    `((:code ((princ data stream))) (,*simple-event*) "bar")

    `((:code ((princ scope stream)
              (princ " " stream)
              (princ data stream)
              (terpri stream)))
      ,*simple-events*
      "/foo/ bar
/baz/ fez
")

    ;; These should result in errors at "format time".
    `((:code ((error "intentional error")))
      (,*simple-event*)
      :error)))

;;; Test suite for `style-programmable/script' class

(deftestsuite style-programmable/script-root (formatting-root)
  ()
  (:documentation
   "Unit tests for the `style-programmable/script' formatting style
class."))

(addtest (style-programmable/script-root
          :documentation
          "Test constructing `style-programmable/script' instances.")
  construct

  (ensure-cases (args expected)
      `(;; Missing initargs.
        (()                                error)
        ((:bindings nil)                   error)

        ;; Invalid combination of initargs.
        ((:script "data" :code (data))     error)

        ;; Invalid initarg types.
        ((:script 5)                       error)
        ((:bindings 5)                     error)

        ;; Invalid initarg values.
        ((:script "reader:error")          format-code-error)
        ((:script "no-such-variable")      format-code-error)
        ((:script "data" :bindings nil)    format-code-error)
        ((:script #P"no-such-file-i-hope") format-code-error)

        ;; Produces warning.
        ((:script "(no-such-function)")    ((rsb.formatting::no-such-function)))

        ;; These are OK.
        ((:script "data")                  (data))
        ((:script ,(make-string-input-stream "data"))
         (data)))

    (case expected
      (error
       (ensure-condition error
         (apply #'make-instance 'style-programmable/script args)))

      (format-code-error
       (ensure-condition format-code-error
         (apply #'make-instance 'style-programmable/script args)))

      (t
       (let ((style (apply #'make-instance 'style-programmable/script
                           args)))
         (ensure-same (style-script style) expected
                      :test #'equal))))))

(addtest (style-programmable/script-root
          :documentation
          "Test some simple cases of formatting events using methods
on `format-event' for `style-programmable/script'.")
  smoke

  (ensure-style-cases (style-programmable/script)
    ;; These are OK.
    '((:script "")                    ()                "")
    `((:script "")                    (,*simple-event*) "")
    `((:script "(princ data stream)") (,*simple-event*) "bar")

    `((:script "(princ scope stream) (princ \" \" stream) (princ data stream) (terpri stream)")
      ,*simple-events*
      "/foo/ bar
/baz/ fez
")

    ;; These should result in errors at "format time".
    `((:script "(error \"intentional error\")")
      (,*simple-event*)
      :error)))

;;; Test suite for `style-programmable/template' class

(deftestsuite style-programmable/template-root (formatting-root)
  ()
  (:documentation
   "Unit tests for the `style-programmable/template' formatting style
class."))

(addtest (style-programmable/template-root
          :documentation
          "Test constructing `style-programmable/template'
instances.")
  construct

  (ensure-cases (args expected)
      `(;; Missing initargs.
        (()                                  error)
        ((:bindings nil)                     error)

        ;; Invalid combination of initargs.
        ((:template "${data}" :code (data))  error)

        ;; Invalid initarg types.
        ((:template 5)                       error)
        ((:bindings 5)                       error)

        ;; Invalid initarg values.
        ((:template "${")                    format-code-error)
        ((:template "${reader:error}")       format-code-error)
        ((:template "${no-such-variable}")   format-code-error)
        ((:template "${data}" :bindings nil) format-code-error)
        ((:template #P"no-such-file-i-hope") format-code-error)

        ;; Produces a warning.
        ((:template "${(no-such-function)}") "${(no-such-function)}")

        ;; These are OK.
        ((:template "literal")               "literal")
        ((:template "${data}")               "${data}")
        ((:template ,(make-string-input-stream "${data}"))
         "${data}"))

    (case expected
      (error
       (ensure-condition error
         (apply #'make-instance 'style-programmable/template args)))

      (format-code-error
       (ensure-condition format-code-error
         (apply #'make-instance 'style-programmable/template args)))

      (t
       (let ((style (apply #'make-instance 'style-programmable/template
                           args)))
         (ensure-same (style-template style) expected
                      :test #'string=))))))

(addtest (style-programmable/template-root
          :documentation
          "Test some simple cases of formatting events using methods
on `format-event' for `style-programmable/template'.")
  smoke

  (ensure-style-cases (style-programmable/template)
    ;; These are OK.
    '((:template "")        ()                "")
    `((:template "")        (,*simple-event*) "")
    `((:template "${data}") (,*simple-event*) "bar")

    `((:template "${scope} ${data}\\n")
      ,*simple-events*
      "/foo/ bar
/baz/ fez
")

    ;; These should result in errors at "format time".
    `((:template "${(error \"intentional error\")}")
      (,*simple-event*)
      :error)))

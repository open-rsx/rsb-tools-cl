;;; style-programmable.lisp --- Unit tests for the programmable formatting style.
;;
;; Copyright (C) 2011, 2012 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This Program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This Program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses>.

(cl:in-package :rsb.formatting.test)


;;; Test suite for `style-programmable' class
;;

(deftestsuite style-programmable-root (formatting-root)
  ()
  (:documentation
   "Unit tests for the `style-programmable' formatting style class."))

(addtest (style-programmable-root
          :documentation
	  "Test constructing `style-programmable' instances.")
  construct

  (ensure-cases (args expected)
      '(;; missing required initargs
	(()                           :error)
	((:bindings nil)              :error)
	;; invalid initarg types
	((:bindings 5)                :error)
	;; invalid initarg values
	((:code (no-such-variable))   :error)
	((:code (data) :bindings nil) :error)
	;; produces a warning
	((:code ((no-such-function))) ((no-such-function)))
	;; these are OK
	((:code ())                   ())
	((:code (data))               (data))
	((:code (skip-event))         (skip-event)))

    (if (eq expected :error)
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

    `((:code ((error "intentional error")))
      (,*simple-event*)
      :error)))


;;; Test suite for `style-programmable/script' class
;;

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
      `(;; missing initargs
	(()                                :error)
	((:bindings nil)                   :error)
	;; invalid combination of initargs
	((:script "data" :code (data))     :error)
	;; invalid initarg types
	((:script 5)                       :error)
	((:bindings 5)                     :error)
	;; invalid initarg values
	((:script "reader:error")          :error)
	((:script "no-such-variable")      :error)
	((:script "data" :bindings nil)    :error)
	((:script #P"no-such-file-i-hope") :error)
	;; produces warning
	((:script "(no-such-function)")    ((rsb.formatting::no-such-function)))
	;; these are OK
	((:script "data")                  (data))
	((:script ,(make-string-input-stream "data"))
	 (data)))

    (if (eq expected :error)
	(ensure-condition error
	  (apply #'make-instance 'style-programmable/script args))
	(let ((style (apply #'make-instance 'style-programmable/script
			    args)))
	  (ensure-same (style-script style) expected
		       :test #'equal)))))

(addtest (style-programmable/script-root
          :documentation
	  "Test some simple cases of formatting events using methods
on `format-event' for `style-programmable/script'.")
  smoke

  (ensure-style-cases (style-programmable/script)
    '((:script "")                    ()                "")
    `((:script "")                    (,*simple-event*) "")
    `((:script "(princ data stream)") (,*simple-event*) "bar")

    `((:script "(princ scope stream) (princ \" \" stream) (princ data stream) (terpri stream)")
      ,*simple-events*
      "/foo/ bar
/baz/ fez
")

    `((:script "(error \"intentional error\")")
      (,*simple-event*)
      :error)))


;;; Test suite for `style-programmable/template' class
;;

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
      `(;; missing initargs
	(()                                  :error)
	((:bindings nil)                     :error)
	;; invalid combination of initargs
	((:template "${data}" :code (data))  :error)
	;; invalid initarg types
	((:template 5)                       :error)
	((:bindings 5)                       :error)
	;; invalid initarg values
	((:template "${")                    :error)
	((:template "${reader:error}")       :error)
	((:template "${no-such-variable}")   :error)
	((:template "${data}" :bindings nil) :error)
	((:template #P"no-such-file-i-hope") :error)
	;; produces a warning
	((:template "${(no-such-function)}") "${(no-such-function)}")
	;; these are OK
	((:template "literal")               "literal")
	((:template "${data}")               "${data}")
	((:template ,(make-string-input-stream "${data}"))
	 "${data}"))

    (if (eq expected :error)
	(ensure-condition error
	  (apply #'make-instance 'style-programmable/template args))
	(let ((style (apply #'make-instance 'style-programmable/template
			    args)))
	  (ensure-same (style-template style) expected
		       :test #'string=)))))

(addtest (style-programmable/template-root
          :documentation
	  "Test some simple cases of formatting events using methods
on `format-event' for `style-programmable/template'.")
  smoke

  (ensure-style-cases (style-programmable/template)
    '((:template "")        ()                "")
    `((:template "")        (,*simple-event*) "")
    `((:template "${data}") (,*simple-event*) "bar")

    `((:template "${scope} ${data}\\n")
      ,*simple-events*
      "/foo/ bar
/baz/ fez
")

    `((:template "${(error \"intentional error\")}")
      (,*simple-event*)
      :error)))

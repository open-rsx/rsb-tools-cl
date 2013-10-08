;;;; options.lisp --- Common functions related to commandline options.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsb.common)

(defun make-common-options (&key
			    show)
  "Return a `clon:group' instance containing common program options."
  (defgroup (:header "General Options")
    (flag   :long-name     "version"
	    :description
	    "Print version information and exit.")
    (flag   :long-name     "help"
	    :short-name    "h"
	    :description
	    "Print this help and exit.")
    (stropt :long-name     "help-for"
	    :argument-name "CATEGORY"
	    :description
	    "Print help for specified categories and exit. This option can be supplied multiple times.")
    (enum   :long-name     "info-stream"
	    :enum          '(:stdout :standard-output
			     :stderr :error-output
			     :none)
	    :default-value :error-output
	    :argument-name "STREAM-NAME"
	    :description
	    "Stream to information messages should be sent.")
    (enum   :long-name     "log-level"
	    :enum          '(:off :trace :info :warn :error)
	    :default-value :warn
	    :argument-name "LEVEL"
	    :description
	    "Controls the amount of generated log output.")
    (path   :long-name     "load"
	    :type          :file
	    :hidden        (not (show-help-for? :advanced-debug
						:show show))
	    :description
	    "Load FILE. This option can be supplied multiple times. Files are loaded in the order in which they appear on the commandline.")
    (stropt :long-name     "eval"
	    :argument-name "SEXP"
	    :hidden        (not (show-help-for? :advanced-debug
						:show show))
	    :description
	    "Evaluate SEXP as Lisp code. This option can be supplied multiple times. Code fragments are evaluated in the order in which they appear on the commandline.")
    (stropt :long-name     "trace"
	    :argument-name "SPEC"
	    :hidden        (not (show-help-for? :advanced-debug
						:show show))
	    :description
	    "Trace specified things. This option can be supplied multiple times to trace multiple things. Each occurrence takes an argument which has to have one of the following forms:
+ \"PACKAGE\" (note the double quotes and uppercase): trace all functions in the package named PACKAGE.
+ function-name (note: no quotes, actual case of the function name): trace the named function.")
    (flag   :long-name     "debug"
	    :hidden        (not (show-help-for? :advanced-debug
						:show show))
	    :description
	    "Enable debugging. This does the following things:
+ Set the log level such that debug output is emitted
+ Enable printing backtraces instead of just condition reports in case of unhandled error conditions.")
    (flag   :long-name     "swank"
	    :hidden        (not (show-help-for? :advanced-debug
						:show show))
	    :description
	    "Start a swank listener (If you don't know what swank is, pretend this option does not exist - or google \"emacs slime\"). Swank will print the port it listens on. In addition, a file named \"./swank-port.txt\" containing the port number is written.")))

(defun make-idl-options ()
  "Return a `clon:group' instance containing IDL-related option
definitions."
  (defgroup (:header "IDL Options")
    (path   :long-name       "idl-path"
	    :short-name      "I"
	    :type            :directory-list
	    :default-value   nil
	    :description
	    "A list of paths from which data definitions should be loaded. This option can be supplied multiple times.")
    (stropt :long-name       "load-idl"
	    :short-name      "l"
	    :argument-name   "FILE-OR-GLOB-EXPRESSION"
	    :description
	    "Load data definition from FILE-OR-GLOB-EXPRESSION. If a glob expression is specified, in addition to the canonical globbing syntax, expressions of the form

  SOMESTUFF/**/MORESTUFF

can be used to search directories recursively. If the file designated by FILE-OR-GLOB-EXPRESSION depend on additional data definition files (i.e. contain \"import\" statements), the list of directories supplied via the --idl-path option is consulted to find these files. This option can be supplied multiple times.")))

(defun make-error-handling-options (&key
				     (show :default))
  "Return a `clon:group' instance program options related to error
handling."
  (declare (ignore show))
  (defgroup (:header "Options related to Handling of Errors")
      (enum    :long-name     "on-error"
	       :enum          '(:abort
				:continue)
	       :default-value :abort
	       :argument-name "STRATEGY"
	       :description
	       "Behavior in case (serious) errors are encountered.

  abort

    Save and cleanup as much as possible, then terminate with unsuccessful result indication.

  continue

    Try to recover from errors and produce best-effort results.")))


;;; Option processing
;;

(defun collect-option-values (&rest spec
			      &key
			      (transform #'(lambda (string)
					     (let ((*read-eval* nil))
					       (read-from-string string))))
			      &allow-other-keys)
  "Return a list of all values that have been supplied for the option
specified by SPEC.

TRANSFORM is applied to all option values."
  (let ((spec (remove-from-plist spec :transform)))
    (iter (for value/string next (apply #'getopt spec))
	  (while value/string)
	  (collect (funcall transform value/string)))))

(defun process-commandline-options (&key
				    (version '(0 1 0))
				    more-versions
				    update-synopsis
				    return)
  "Perform the following commandline option processing:

+ if --version has been supplied, print version information and call
  RETURN or exit.

+ if --help has been supplied, print a help text and call RETURN or
  exit.

+ if --trace has been supplied (at least once), trace the packages or
  functions specified in the arguments to --trace.

+ if --debug has been supplied, keep debugger enabled and adjust
  log-level, otherwise disable debugger

+ if --swank is supplied, start a swank server and write its port to
  ./swank-port.txt"
  ;; Create a new global context.
  (make-context)

  ;; Process output-related options
  (setf *info-output*
	(ecase (getopt :long-name "info-stream")
	  ((:none)                          nil)
	  ((:cout :stdout :standard-output) *standard-output*)
	  ((:cerr :stderr :error-output)    *error-output*)))

  ;; Process logging-related options.
  (let ((level (getopt :long-name "log-level")))
    (unless (eq level :off)
      (setf (log-level) level)))

  ;; Process --trace options.
  (let ((trace-specs
	 (iter (for trace-spec next (getopt :long-name "trace"))
	       (while trace-spec)
	       (collect (read-from-string trace-spec)))))
    (trace-things trace-specs))

  ;; Process --debug option.
  (unless (getopt :long-name "debug")
    (disable-debugger))

  ;; Process --load options.
  (with-compilation-unit ()
    (map nil #'load (collect-option-values
		     :long-name "load"
		     :transform #'identity)))

  ;; Process --eval options.
  (map nil #'eval (collect-option-values :long-name "eval"))

  ;; Process --swank option.
  (when (getopt :long-name "swank")
    (start-swank))

  ;; Load specified RSB plugins, potentially updating the option
  ;; synopsis afterwards.
  ;; (rsb::load-plugins)
  ;; (when update-synopsis
  ;;   (funcall update-synopsis)
  ;;
  ;;   ;; Create a new global context.
  ;;   (make-context))

  ;; Process --version option.
  (when (getopt :long-name "version")
    (print-version version *standard-output*
		   :more-versions more-versions)
    (terpri *standard-output*)
    (if return
	(funcall return)
	(exit 0)))

  ;; Process --help and --help-for options.
  (let ((show)
	(help (getopt :long-name "help")))
    (iter (for category next (getopt :long-name "help-for"))
	  (while category)
	  (when (string= category "all")
	    (setf show t)
	    (terminate))
	  (push (make-keyword (string-upcase category)) show))
    (when show
      (funcall update-synopsis :show show)
      (make-context))
    (when (or help show)
      (help)
      (if return
	  (funcall return)
	  (exit 0)))))

(defun process-error-handling-options ()
  "Process the \"on-error\" commandline option mapping abort to
`abort/verbose' and continue to `continue/verbose' and returning the
respective function."
  (ecase (getopt :long-name "on-error")
    (:abort    #'abort/signal)
    (:continue #'continue/verbose)))


;;; Instantiation spec parsing
;;

(defun parse-instantiation-spec (string)
  "Parse STRING as an instantiation specification of one of the forms

  KIND KEY1 VALUE1 KEY2 VALUE2 ...

and

  KIND VALUE1

and return the result as a list."
  (maybe-expand-instantiation-spec
   (with-input-from-string (stream string)
     (iter (for token in-stream stream)
	   (collect
	       (if (and (first-iteration-p)
			(not (keywordp token)))
		   (make-keyword (string-upcase (string token)))
		   token))))))

(defun simple-instantiation-spec? (spec)
  "Return non-nil if SPEC is a \"simple\" instantiation specification
of the form (KIND SOLE-ARGUMENT)."
  (and (length= 2 spec) (keywordp (first spec))))

(defun maybe-expand-instantiation-spec (spec)
  "Expand SPEC into a full instantiation specification if it is a
simple specification. Otherwise, just return SPEC."
  (if (simple-instantiation-spec? spec)
      (cons (first spec) spec)
      spec))

;;; main.lisp ---
;;
;; Copyright (C) 2011 Jan Moringen
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

(in-package :rsb.tools.logger)

(defun make-help-string ()
  "Return a help that explains the commandline option interface."
  (with-output-to-string (stream)
    (format stream "Show events exchanged on the RSB channel ~
designated by URI. Events can be filtered and displayed in several ~
ways which can be controlled using the --filter and --style options.

URI designates the channel for which events should be received and ~
logged and the transport that should be used to attach to channel. A ~
URI of the form

  ")
    (print-uri-help stream)))

(defun make-filter-help-string ()
  "Return a help string that explains how to specify filters and lists
the available filters."
  (with-output-to-string (stream)
    (format stream "Specify a filter that received events have to ~
match in order to be processed rather than discarded. This option can ~
be supplied multiple times in which case events have to match all ~
specified filters. Each SPEC has to be of one of the forms

  KIND | KIND SINGLE-VALUE | KIND KEY1 VALUE1 KEY2 VALUE2 ...

where keys and values depend on KIND and may be mandatory in some ~
cases. Examples (note that the single quotes have to be included only ~
when used within a shell):

  --filter 'origin \"EAEE2B00-AF4B-11E0-8930-001AA0342D7D\"'
  --filter 'regex \".*foo[0-9]+\"'
  --filter 'regex :regex \".*foo[0-9]+\"' (equivalent)
  -f 'xpath :xpath \"node()/@foo\" :fallback-policy :do-not-match'

The following filters are currently available (paragraph headings ~
correspond to respective KIND):

")
    (print-filter-help stream)))

(defun make-style-help-string ()
  "Return a help string that explains how to specify a formatting
style and its parameters."
  (with-output-to-string (stream)
    (format stream "Specify a formatting style that should be used to ~
print events. Each SPEC has to be of the form

  KIND KEY1 VALUE1 KEY2 VALUE2 ...

where keys and values are optional and depend on KIND. Examples (note ~
that the single quotes have to be included only when used within a ~
shell):

  --style detailed
  -s compact
  --style 'compact :separator \"|\"'

The following formatting styles are currently available:

")
    (print-classes-help-string (style-classes) stream)))

(defun make-examples-string (&key
			     (program-name "logger"))
  "Make and return a string containing usage examples of the program."
  (format nil "~A

  Use all enabled transports with their respective default ~
configuration to access the bus. Receive and display all events ~
exchanged on the entire bus (since the channel designated by the root ~
scope, \"/\", is implicitly used).

~:*~A spread://localhost:4811

  Use the Spread daemon listening on port 4811 on localhost to connect ~
to the bus. Since no scope is specified, receive and print all events ~
exchanged on the entire bus.

~:*~A -f 'regex :regex \"^mypattern\" :fallback-policy :do-not-match' ~
--style detailed spread:/my/channel

  Use the default configuration of the Spread transport to connect to ~
the bus. Receive events on the channel designated by ~
\"/my/channel\" (and sub-channels) the payloads of which match the ~
regular expression \"^mypattern\". Display matching event using the ~
\"detailed\" display style.
"
	  program-name))

(defun update-synopsis (&key
			(show :default))
  "Create and return a commandline option tree."
  (make-synopsis
   ;; Basic usage and specific options.
   :postfix "[URI]"
   :item    (make-text :contents (make-help-string))
   :item    (make-common-options :show show)
   :item    (defgroup (:header "Logging Options"
		       :hidden (and (listp show)
				    (not (member :logging show))))
	      (stropt :long-name       "filter"
		      :short-name      "f"
		      :argument-name   "SPEC"
		      :description
		      (make-filter-help-string))
	      (stropt :long-name       "style"
		      :short-name      "s"
		      :default-value   "compact"
		      :argument-name   "SPEC"
		      :description
		      (make-style-help-string)))
   :item    (defgroup (:header "IDL Options")
	      (path   :long-name       "idl-path"
		      :short-name      "I"
		      :type            :directory-list
		      :default-value   nil
		      :description
		      "A list of paths from which data definitions should be loaded. This option can be supplied multiple times.")
	      (stropt :long-name       "load-idl"
		      :short-name      "l"
		      :argument-name   "FILE"
		      :description
		      "Load data definition from FILE. If FILE depends on additional data definition files (i.e. contains \"import\" statements), the list of directories supplied via the --idl-path option is consulted to find these files. This option can be supplied multiple times."))
   ;; Append RSB options.
   :item    (make-options
	     :show? (or (eq show t)
			(and (listp show) (member :rsb show))))
   ;; Append examples.
   :item    (defgroup (:header "Examples")
	      (make-text :contents (make-examples-string)))))

(defun existing-directory-or-lose (pathname)
  "Signal an error unless PATHNAME designates an existing directory."
  (if-let ((truename (probe-file pathname)))
    (when (or (pathname-name truename)
	      (pathname-type truename))
      (error "~@<Not a directory: ~A.~@:>" truename))
    (error "~@<Directory does not exist: ~A.~@:>" pathname)))

(defun main ()
  "Entry point function of the cl-rsb-tools-logger system."
  (update-synopsis)
  (setf *default-configuration* (options-from-default-sources))
  (process-commandline-options
   :version         (cl-rsb-tools-logger-system:version/list)
   :update-synopsis #'update-synopsis
   :return          #'(lambda () (return-from main)))

  ;; Validate commandline options.
  (when (> (length (remainder)) 1)
    (error "~@<Specify at most one URI (also, options cannot follow ~
the URI argument).~@:>"))

  (with-logged-warnings

    ;; Extend data definition source path.
    (iter (for paths next (getopt :long-name "idl-path"))
	  (while paths)
	  (iter (for path in paths)
		(existing-directory-or-lose path)
		(pushnew path pbf:*proto-load-path*)))

    ;; Load specified data definitions.
    (iter (for spec next (getopt :long-name "load-idl"))
	  (while spec)
	  (load-idl spec :auto))

    ;; Create a reader and start the receiving and printing loop.
    (let* ((uri         (or (first (remainder)) "/"))
	   (filters     (iter (for spec next (getopt :long-name "filter"))
			      (while spec)
			      (collect (apply #'rsb.filter:filter
					      (parse-instantiation-spec spec)))))
	   (converters  (iter (for (wire-type . converter) in (default-converters))
			      (collect
				  (cons wire-type
					(if (listp converter)
					    (append converter '(:fundamental-null))
					    converter)))))
	   (event-style (bind (((class &rest args)
				(parse-instantiation-spec
				 (getopt :long-name "style"))))
			  (apply #'make-instance (find-style-class class)
				 args))))
      (log1 :info "Using URI ~S" uri)
      (with-reader (reader uri
			   :transports '((:spread :expose-wire-schema? t
					  &inherit))
			   :converters converters)
	(setf (receiver-filters reader) filters)
	(log1 :info "Created reader ~A" reader)

	(with-interactive-interrupt-exit ()
	  (iter (for event next (receive reader :block? t))
		(format-event event event-style *standard-output*)))))))

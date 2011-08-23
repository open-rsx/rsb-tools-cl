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
    (print-uri-help stream)
    (format stream
	    "
Examples:

  ~A --rsb-plugins-load cl-spread spread://localhost:4811
  ~:*~A --rsb-plugins-load cl-spread,xpath spread:
"
	    ;; (progname)
	    "rsb-logger")))

(defun make-filter-help-string ()
  "Return a help string that explains how to specify filters and lists
the available filters. "
  (with-output-to-string (stream)
    (format stream "Specify a filter that received events have to ~
match in order to be processed rather than discarded. This option can ~
be supplied multiple times in which case events have to match all ~
specified filters. Each SPEC has to be of the form

  KIND KEY1 VALUE1 KEY2 VALUE2 ...

where keys and values depend on KIND and may be optional in some ~
cases. Examples (note that the single quotes have to be included only ~
when used within a shell):

  --filter 'origin \"EAEE2B00-AF4B-11E0-8930-001AA0342D7D\"'
  --filter 'regex \".*foo[0-9]+\"'

The following filters are currently available (paragraph headings ~
correspond to respective KIND):

")
    (print-filter-help stream)))

(defun update-synopsis (&key
			(show :default))
  "Create and return a commandline option tree."
  (make-synopsis
   :postfix "[URI]"
   :item    (make-text :contents (make-help-string))
   :item    (make-common-options)
   :item    (defgroup (:header "Logging Options"
		       :hidden (and (listp show)
				    (not (member :logging show))))
	      (stropt :short-name      "f"
		      :long-name       "filter"
		      :description
		      (make-filter-help-string))
	      (enum   :short-name      "s"
		      :long-name       "style"
		      :enum            (map 'list #'first
					    (format-styles 'format-event))
		      :default-value   :compact
		      :description
		      (format nil "The style to use when printing events. The following styles are available:

~{~{~(~A~)~&~2T~@<~@;~A~:>~}~^~&~}"
			      (format-styles 'format-event))))
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
		      "Load data definition from FILE. If FILE depends on additional data definition files (i.e. contains \"import\" statements), the list directories supplied via the --idl-path option is consulted to find these files. This option can be supplied multiple times."))
   ;; Append RSB options.
   :item   (rsb:make-options
	    :show? (or (eq show t)
		       (and (listp show) (member :rsb show))))))

(defun main ()
  "Entry point function of the cl-rsb-tools-logger system."
  (update-synopsis)
  (setf rsb:*default-configuration* (cons '((:transport :spread :converter)
					    . (:fundamental-bytes :fundamental-utf-8-string :fundamental-acsii-string :protocol-buffer)) ;;; TODO(jmoringe):
					  (rsb:options-from-default-sources)))

  (process-commandline-options
   :update-synopsis #'update-synopsis
   :return          (lambda () (return-from main)))

  (with-logged-warnings

    ;; Extend data definition source path.
    (iter (for paths next (getopt :long-name "idl-path"))
	  (while paths)
	  (iter (for path in paths)
		(pushnew path pbf:*proto-load-path*)))

    ;; Load specified data definitions.
    (iter (for spec next (getopt :long-name "load-idl"))
	  (while spec)
	  (load-idl spec :auto))

    (let* ((uri         (first (com.dvlsoft.clon:remainder)))
	   (filters     (iter (for spec next (getopt :long-name "filter"))
			      (while spec)
			      (collect
				  (make-filter (parse-filter-spec spec)))))
	   (event-style (getopt :long-name "style")))
      (log5:log-for log5:info "Using URI ~S" uri)

      (rsb:with-reader (reader (or uri "/"))
	(setf (receiver-filters reader) filters)
	(log5:log-for log5:info "Created reader ~A" reader)

	(with-interactive-interrupt-exit
	  (iter (for event next (rsb:receive reader :block? t))
		(format-event event event-style *standard-output*)))))))

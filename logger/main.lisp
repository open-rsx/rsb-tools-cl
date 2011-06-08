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

(in-package :rsb-logger)

(defun update-synopsis ()
  "TODO(jmoringe): document"
  (make-synopsis
   :postfix "[URI]"
   :item    (make-text
	     :contents (format nil "URI designates the channel for which ~
events should be received and logged and the transport that should be ~
used to attach to channel. ~A The following URI schemas designate ~
supported transports: ~{~(~A~)~^, ~}.

Examples:

  ~A --rsb-plugins-load cl-spread spread://localhost:4811
  ~:*~A --rsb-plugins-load cl-spread,xpath spread:"
			       (documentation 'rsb::uri->scope-and-options 'function)
			       (remove-duplicates
				(apply #'append
				       (map 'list (compose #'rsb.transport:connector-schemas
							   #'second)
					    (rsb.transport:transport-classes))))
			       ;; (progname)
			       "rsb-logger"))
   :item    (defgroup (:header "General Options")
	      (flag    :short-name     "h"
		       :long-name      "help"
		       :description
		       "Print this help and exit.")
	      (flag    :short-name     "d"
		       :long-name      "debug"
		       :description
		       "Enable debugging."))
   :item    (defgroup (:header "Logging Options")
	      (stropt  :short-name      "f"
		       :long-name       "filter"
		       :description
		       "TODO")
	      (enum    :short-name      "t"
		       :long-name       "target"
		       :enum            '(:standard-output)
		       :default-value   :standard-output
		       :description
		       "TODO")
	      (enum    :short-name      "l"
		       :long-name       "detail-level"
		       :enum            (format-styles 'format-event)
		       :default-value    :line
		       :description
		       "Level of detail when printing events."))
   ;; RSB Options
   :item   (rsb::make-options))

  ;; Create a new global context.
  (make-context))

(defun construct-filter (spec)
  "TODO(jmoringe): document"
  (with-input-from-string (stream spec)
    (apply #'rsb.filter:filter
	   (iter (for arg in-stream stream)
		 (collect arg)))))

(defun main ()
  "TODO(jmoringe): document"
  (update-synopsis)
  (setf rsb:*default-configuration* (cons '((:transport :spread :converter)
					    . (:fundamental-bytes :fundamental-string :protocol-buffer))
					  (rsb:options-from-default-sources)))

  (when (getopt :long-name "debug")
    (log5:debugging 'log5:info+))

  (rsb::load-plugins)
  (update-synopsis)

  (when (getopt :long-name "help")
    (help)
    (return-from main))

  ;; (format t "Using filter ~A~%" (construct-filter (getopt :long-name "filter")))

  (let* ((uri         (first (com.dvlsoft.clon:remainder)))
	 (event-style (getopt :long-name "detail-level"))
	 (terminate? nil))
    (unless uri
      (format t "Specify URI~%")
      (return-from main))

    (log5:log-for log5:info "Using URI ~S" uri)
    (log5:log-for log5:info "Installing SIGINT handler ~S" sb-unix:SIGINT)
    (sb-unix::enable-interrupt
     sb-unix:SIGINT
     #'(lambda (signal info context)
	 (declare (ignore info context))
	 (setf terminate? t)
	 (log5:log-for log5:info "Received signal ~D; Shutting down ..." signal)))

    ;(handler-case
	(rsb:with-reader (reader uri)
	  (log5:log-for log5:info "Created reader ~A" reader)
	  (iter (until terminate?)
		(for event next (rsb:receive reader :block? t))
		(format-event event event-style *standard-output*)))
      ;(error (condition)
	;(format *error-output* "Error: ~A~%" condition)
	;))
	))

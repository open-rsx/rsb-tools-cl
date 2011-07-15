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
    (format stream "URI designates the channel for which events ~
should be received and logged and the transport that should be used ~
to attach to channel. A URI of the form

  ")
    (print-uri-help-string stream)
    (format stream
	    "
Examples:

  ~A --rsb-plugins-load cl-spread spread://localhost:4811
  ~:*~A --rsb-plugins-load cl-spread,xpath spread:
"
	    ;; (progname)
	    "rsb-logger")))

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
		      "TODO")
	      (enum   :short-name      "s"
		      :long-name       "style"
		      :enum            (format-styles 'format-event)
		      :default-value   :compact
		      :description
		      "The style to use when printing events."))
   ;; Append RSB options.
   :item   (rsb:make-options
	    :show? (or (eq show t)
		       (and (listp show) (member :rsb show))))))

(defun make-filter-tree (spec)
  "Construct and return a filter tree according to SPEC."
  (with-input-from-string (stream spec)
    (apply #'rsb.filter:filter
	   (iter (for arg in-stream stream)
		 (collect arg)))))

(defun main ()
  "Entry point function of the cl-rsb-tools-logger system."
  (update-synopsis)
  (setf rsb:*default-configuration* (cons '((:transport :spread :converter)
					    . (:fundamental-bytes :fundamental-utf-8-string :fundamental-acsii-string :protocol-buffer)) ;;; TODO(jmoringe):
					  (rsb:options-from-default-sources)))

  (process-commandline-options
   :update-synopsis #'update-synopsis
   :return          (lambda () (return-from main)))

  (let* ((uri         (first (com.dvlsoft.clon:remainder)))
	 (event-style (getopt :long-name "style")))
    (log5:log-for log5:info "Using URI ~S" uri)

    (rsb:with-reader (reader (or uri "/"))
      (log5:log-for log5:info "Created reader ~A" reader)

      (catch 'terminate
	(log5:log-for log5:info "Installing SIGINT handler ~S" sb-unix:SIGINT)
	(sb-unix::enable-interrupt
	 sb-unix:SIGINT
	 #'(lambda (signal info context)
	     (declare (ignore info context))
	     (log5:log-for log5:info "Received signal ~D; Shutting down ..." signal)
	     (throw 'terminate nil)))

	(iter (while t)
	      (for event next (rsb:receive reader :block? t))
	      (format-event event event-style *standard-output*))))))

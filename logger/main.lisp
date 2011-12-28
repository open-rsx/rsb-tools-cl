;;; main.lisp --- Entry point of the logger tool.
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

(cl:in-package :rsb.tools.logger)

(defun update-synopsis (&key
			(show :default))
  "Create and return a commandline option tree."
  (make-synopsis
   ;; Basic usage and specific options.
   :postfix "[URI]"
   :item    (make-text :contents (make-help-string :show show))
   :item    (make-common-options :show show)
   :item    (defgroup (:header "Logging Options"
		       :hidden (not (show-help-for?
				     '(:logging :filters :styles :columns :quantities)
				     :default t
				     :show    show)))
	      (stropt :long-name       "filter"
		      :short-name      "f"
		      :argument-name   "SPEC"
		      :description
		      (make-filter-help-string :show show))
	      (stropt :long-name       "style"
		      :short-name      "s"
		      :default-value   "compact"
		      :argument-name   "SPEC"
		      :description
		      (make-style-help-string :show show)))
   :item    (make-idl-options)
   ;; Append RSB options.
   :item    (make-options
	     :show? (show-help-for? :rsb :show show))
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
					(if (and (listp converter)
						 (not (member :fundamental-null converter)))
					    (append converter '(:fundamental-null))
					    converter)))))
	   (event-style (bind (((class &rest args)
				(parse-instantiation-spec
				 (getopt :long-name "style"))))
			  (apply #'make-instance (find-style-class class)
				 args))))

      (with-print-limits (*standard-output*)
	(log1 :info "Using URI ~S" uri)

	(with-reader (reader uri
			     :transports '((:spread :expose-wire-schema? t
					    &inherit)
					   (:socket :expose-wire-schema? t
					    &inherit))
			     :converters converters)
	  (setf (receiver-filters reader) filters)
	  (log1 :info "Created reader ~A" reader)

	  (with-interactive-interrupt-exit ()
	    (iter (for event next (receive reader :block? t))
		  (format-event event event-style *standard-output*))))))))

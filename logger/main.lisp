;;;; main.lisp --- Entry point of the logger tool.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsb.tools.logger)

(defun update-synopsis (&key
			(show :default))
  "Create and return a commandline option tree."
  (make-synopsis
   ;; Basic usage and specific options.
   :postfix "[URI]"
   :item    (make-text :contents (make-help-string :show show))
   :item    (make-common-options :show show)
   :item    (make-error-handling-options :show show)
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

(defun main ()
  "Entry point function of the cl-rsb-tools-logger system."
  (update-synopsis)
  (setf *default-configuration* (options-from-default-sources))
  (process-commandline-options
   :version         (cl-rsb-tools-logger-system:version/list :commit? t)
   :update-synopsis #'update-synopsis
   :return          #'(lambda () (return-from main)))


  ;; Validate commandline options.
  (when (> (length (remainder)) 1)
    (error "~@<Specify at most one URI (also, options cannot follow ~
the URI argument).~@:>"))

  (with-logged-warnings
    ;; Load IDLs as specified on the commandline.
    (process-idl-options)

    ;; Create a reader and start the receiving and printing loop.
    (let* ((error-policy (maybe-relay-to-thread
			  (process-error-handling-options)))
	   (uri           (or (first (remainder)) "/"))
	   (filters      (iter (for spec next (getopt :long-name "filter"))
			       (while spec)
			       (collect (apply #'rsb.filter:filter
					       (parse-instantiation-spec spec)))))
	   (converters   (iter (for (wire-type . converter) in (default-converters))
			       (collect
				   (cons wire-type
					 (if (and (listp converter)
						  (not (member :fundamental-null converter)))
					     (append converter '(:fundamental-null))
					     converter)))))
	   (event-style  (let+ (((class &rest args)
				 (parse-instantiation-spec
				  (getopt :long-name "style"))))
			   (apply #'make-instance (find-style-class class)
				  args))))

      (with-print-limits (*standard-output*)
	(log1 :info "Using URI ~S" uri)

	(with-error-policy (error-policy)
	  (with-interactive-interrupt-exit ()
	    (with-reader (reader uri
				 :transports '((t :expose (:rsb.transport.wire-schema
							   :rsb.transport.payload-size)
						  &inherit))
				 :converters converters)
	      (hooks:add-to-hook (rsb:participant-error-hook reader)
				 error-policy) ;;; TODO(jmoringe, 2012-08-10): support this in with-reader
	      (setf (receiver-filters reader) filters)
	      (log1 :info "Created reader ~A" reader)

	      (iter (for event next (receive reader :block? t))
		    (restart-case
			(format-event event event-style *standard-output*)
		      (continue (&optional condition)
			:report (lambda (stream)
				  (format stream "~@<Ignore the ~
failure for ~A and continue processing.~@:>"
					  event))
			(declare (ignore condition))))))))))))

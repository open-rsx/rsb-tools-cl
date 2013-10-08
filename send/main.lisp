;;;; main.lisp --- Entry point of the send tool.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsb.tools.send)

(defun make-help-string (&key
			 (show :default))
  "Return a help that explains the commandline option interface."
  (with-output-to-string (stream)
    (format stream "Send an event constructed according to EVENT-SPEC ~
to listeners on scopes specified by DESTINATION-URI.

EVENT-SPEC is parsed as string when surrounded with double-quotes and ~
as integer or float number when consisting of digits without and with ~
decimal point respectively.

If EVENT-SPEC is the single character \"-\", the entire \"contents\" ~
of standard input (until end of file) is read as a string and used as ~
argument for the method send.

DESTINATION-URI designates the destination scope to which the event ~
should be sent and the transport configuration which should be used ~
for sending the event.

")
    (with-abbreviation (stream :uri show)
      (progn
	(format stream "A DESTINATION-URI is of the form

  ")
	(print-uri-help stream :uri-var "DESTINATION-URI")))))

(defun make-examples-string (&key
			     (program-name #+does-not-work (progname) "send"))
  "Make and return a string containing usage examples of the program."
  (format nil
	  "~A 5 'spread://localhost:4811/whoever/listens/here'

  Use the spread transport to send an event containing the integer ~
\"5\".

~:*~A 5 'spread:/same/as/before?name=4803'

  Like the previous example, but use the \"daemon name\" option of the ~
Spread transport instead of specifying host and port.

cat my-data.txt | ~:*~A - 'socket:/printer'

  Send the content of the file \"my-data.txt\" to the listener at ~
scope \"/printer\" using the socket transport (with its default ~
configuration). This form can only be used for sending string ~
payloads.
"
	  program-name))

(defun update-synopsis (&key
			(show :default))
  "Create and return a commandline option tree."
  (make-synopsis
   ;; Basic usage and specific options.
   :postfix "EVENT-SPEC [DESTINATION-URI]"
   :item    (make-text :contents (make-help-string :show show))
   :item    (make-common-options :show show)
   :item    (make-error-handling-options :show show)
   :item    (defgroup (:header "Event Options"
		       :hidden (not (show-help-for? '(:event)
						    :default t
						    :show    show)))
	      (stropt :long-name       "method"
		      :short-name      "m"
		      :argument-name   "METHOD"
		      :description
		      "Set the method field of the event being sent to METHOD. Default behavior is sending an event without method field.")
	      (stropt :long-name       "meta-data"
		      :short-name      "D"
		      :argument-name   "NAME=VALUE"
		      :description
		      "Set the meta-data item NAME to VALUE in the event being sent. This option can be specified multiple times for distinct NAMEs.")
	      (stropt :long-name       "timestamp"
		      :short-name      "T"
		      :argument-name   "NAME=YYYY-MM-DD[THH:MM:SS[.µµµµµµ[+ZH:ZM]]]"
		      :description
		      "Set the timestamp named NAME to the timestamp YYYY-MM-DD[THH:MM:SS[.µµµµµµ[+ZH:ZM]]] in the event being sent. This option can be specified multiple times for distinct NAMEs.")
	      (stropt :long-name       "cause"
		      :short-name      "c"
		      :argument-name   "PARTICIPANT-ID:SEQUENCE-NUMBER"
		      :description
		      "Add the event id described by PARTICIPANT-ID:SEQUENCE-NUMBER to the cause vector of the event being sent. This option can be specified multiple times."))
   ;; Append IDL options
   :item    (make-idl-options)
   ;; Append RSB options.
   :item    (make-options
	     :show? (or (eq show t)
			(and (listp show) (member :rsb show))))
   ;; Append examples.
   :item    (defgroup (:header "Examples")
	      (make-text :contents (make-examples-string)))))

(defun parse-event-spec (spec)
  "Parse SPEC as Lisp object treating the empty string specially."
  (cond
    ((emptyp spec)
     rsb.converter:+no-value+)

    ((string= spec "-")
     (with-output-to-string (stream)
       (copy-stream *standard-input* stream)))

    ((starts-with-subseq "#p" spec :test #'char-equal)
     (read-file-into-string (parse-namestring (subseq spec 2))))

    (t
     (let+ (((&values value consumed)
	     (read-from-string spec)))
       (unless (= consumed (length spec))
	 (error "~@<Junk at end of argument string: ~S.~@:>"
		(subseq spec consumed)))
       value))))

(defun parse-pair (pair
		   &key
		   (separator        #\=)
		   (first-transform  #'identity)
		   (second-transform #'identity))
  (let ((index (or (position separator pair)
		   (error "~@<~S is not of the form KEY~CVALUE.~@:>"
			  pair separator))))
    (list (funcall first-transform (subseq pair 0 index))
	  (funcall second-transform (subseq pair (1+ index))))))

(defun parse-meta-data (value)
  (parse-pair value :first-transform #'make-keyword))

(defun parse-timestamp (value)
  (parse-pair value :first-transform  #'make-keyword
		    :second-transform #'local-time:parse-timestring))

(defun parse-cause (value)
  (apply #'cons
	 (parse-pair value :separator        #\:
			   :first-transform  #'uuid:make-uuid-from-string
			   :second-transform #'parse-integer)))

(defun main ()
  "Entry point function of the cl-rsb-tools-send system."
  (update-synopsis)
  (setf *default-configuration* (options-from-default-sources))
  (process-commandline-options
   :version         (cl-rsb-tools-send-system:version/list :commit? t)
   :update-synopsis #'update-synopsis
   :return          #'(lambda () (return-from main)))
  (enable-swank-on-signal)

  (unless (length= 2 (remainder))
    (error "~@<Supply event specification and destination URI.~@:>"))

  (with-logged-warnings
    (let+ ((error-policy (maybe-relay-to-thread
			  (process-error-handling-options)))
	   (method     (when-let ((value (getopt :long-name "method")))
			 (make-keyword value)))
	   (meta-data  (iter (for value next (getopt :long-name "meta-data"))
			     (while value)
			     (appending (parse-meta-data value))))
	   (timestamps (iter (for value next (getopt :long-name "timestamp"))
			     (while value)
			     (appending (parse-timestamp value))))
	   (causes     (iter (for value next (getopt :long-name "cause"))
			     (while value)
			     (collect (parse-cause value))))
	   ((event-spec &optional (destination "/")) (remainder))
	   (payload (parse-event-spec event-spec)))

      (log1 :info "Using URI ~S payload ~A"
	    destination payload)
      (with-interactive-interrupt-exit ()
	(with-error-policy (error-policy)
	  (with-informer (informer destination t)
	    (hooks:add-to-hook (participant-error-hook informer)
			       error-policy) ;;; TODO(jmoringe, 2012-08-09): support in with-informer
	    (apply #'send informer payload
		   (nconc (when method     (list :method     method))
			  (when timestamps (list :timestamps timestamps))
			  (when causes     (list :causes     causes))
			  meta-data))))))))

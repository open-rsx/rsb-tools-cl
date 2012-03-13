;;; main.lisp --- Entry point of the send tool.
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
scope \"/printer\" using the socket transform (with its default ~
configuration). This form can only be used for sending string ~
payloads.
"
	  program-name))

(defun update-synopsis (&key
			(show :default))
  "Create and return a commandline option tree."
  (make-synopsis
   ;; Basic usage and specific options.
   :postfix "EVENT-SPEC DESTINATION-URI"
   :item    (make-text :contents (make-help-string :show show))
   :item    (make-common-options :show show)
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

(defun main ()
  "Entry point function of the cl-rsb-tools-send system."
  (update-synopsis)
  (setf *default-configuration* (options-from-default-sources))
  (process-commandline-options
   :version         (cl-rsb-tools-send-system:version/list)
   :update-synopsis #'update-synopsis
   :return          #'(lambda () (return-from main)))
  (enable-swank-on-signal)

  (unless (length= 2 (remainder))
    (error "~@<Supply event specification and destination URI.~@:>"))

  (with-logged-warnings
    (let+ (((event-spec destination) (remainder))
	   (payload (parse-event-spec event-spec)))

      (log1 :info "Using URI ~S payload ~A"
	    destination payload)
      (with-interactive-interrupt-exit ()
	(with-informer (informer destination t)
	  (send informer payload))))))

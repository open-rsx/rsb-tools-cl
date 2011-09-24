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

(in-package :rsb.tools.call)

(defun make-help-string ()
  "Return a help that explains the commandline option interface."
  (with-output-to-string (stream)
    (format stream "Call METHOD of the server at SERVER-URI with ~
argument ARG.

ARG is parsed as string when surrounded with double-quotes and as ~
integer or float number when consisting of digits without and with ~
decimal point respectively.
If ARG is the single character -, the entire \"contents\" of standard ~
input (until end of file) is read as a string and used as argument for ~
the method call.
If ARG is the empty string, i.e. the call specification is of the form ~
SERVER-URI/METHOD(), the method is called without argument.

SERVER-URI designates the root scope of the remote server and the ~
transport that should be used. A URI of the form

  ")
    (print-uri-help stream :uri-var "SERVER-URI")))

(defun make-examples-string (&key
			     (program-name #+does-not-work (progname) "call"))
  "Make and return a string containing usage examples of the program."
  (format nil
	  "~A spread://localhost:4811/my/interface/method(5)

  Use the spread transport to call the method \"method\" of the server ~
at \"/my/inferface\" passing it the integer argument \"5\".

~:*~A /my/interface/noarg()

  Use the default transport configuration to call the \"noarg\" method ~
of the server at scope \"/my/inferface\" without argument.

~:*~A /remotecontrol/stop(\"now\")

  Use the default transport configuration to call the \"stop\" method ~
of the server at scope \"/remotecontrol\" passing it the string ~
argument \"now\".

cat my-arg.txt | ~:*~A socket:/printer/print(-)

  Call the \"print\" method of the server at scope \"/printer\" using ~
the socket transform (with its default configuration) using the ~
content of the file \"my-arg.txt\" as argument of the call. This only ~
works if the called method accepts an argument of type string.
"
	  program-name))

(defun update-synopsis (&key
			(show :default))
  "Create and return a commandline option tree."
  (make-synopsis
   ;; Basic usage and specific options.
   :postfix "SERVER-URI/METHOD(ARG)"
   :item    (make-text :contents (make-help-string))
   :item    (make-common-options :show show)
   ;; Append RSB options.
   :item    (make-options
	     :show? (or (eq show t)
			(and (listp show) (member :rsb show))))
   ;; Append examples.
   :item    (defgroup (:header "Examples")
	      (make-text :contents (make-examples-string)))))

(defun parse-argument (string)
  "Parse STRING as Lisp object treating the empty string specially."
  (cond
    ((emptyp string)
     rsb.converter:+no-value+)
    ((string= string "-")
     (with-output-to-string (stream)
       (copy-stream *standard-input* stream)))
    (t
     (bind (((:values value consumed)
	     (read-from-string string)))
       (unless (= consumed (length string))
	 (error "~@<Junk at end of argument string: ~S.~@:>"
		(subseq string consumed)))
       value))))

(defun main ()
  "Entry point function of the cl-rsb-tools-call system."
  (update-synopsis)
  (setf *default-configuration* (options-from-default-sources))
  (process-commandline-options
   :version         (cl-rsb-tools-call-system:version/list)
   :update-synopsis #'update-synopsis
   :return          #'(lambda () (return-from main)))
  (enable-swank-on-signal)

  (unless (length= 1 (remainder))
    (error "~@<Supply call specification of the form ~
SERVER-URI/METHOD(ARG).~@:>"))

  (with-logged-warnings
    (bind ((spec (first (remainder)))
	   ((:values server-uri method arg)
	    (cl-ppcre:register-groups-bind (server-uri method arg)
		("([a-zA-Z0-9/:&=;]*)/([a-zA-Z0-9]+)\\((.*)\\)" spec)
	      (values server-uri method (parse-argument arg)))))

      (unless (and server-uri method arg)
	(error "~@<Parse error in call specification ~S.~@:>"
	       spec))

      (log1 :info "Using URI ~S method ~S arg ~A"
	    server-uri method arg)
      (with-interactive-interrupt-exit ()
	(with-remote-server (server server-uri)
	  (bind ((reply  (multiple-value-list
			  (call server method arg)))
		 (reply? (not (null reply)))
		 (reply  (when reply?
			   (first reply))))
	    (when reply?
	      (format t "~S~%" reply))))))))

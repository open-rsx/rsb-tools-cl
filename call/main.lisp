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

SERVER-URI designates the root scope of the remote server and the ~
transport that should be used. A URI of the form

  ")
    (print-uri-help stream :uri-var "SERVER-URI")
    (format stream
	    "
Examples:

  ~A spread://localhost:4811/my/interface/method(5)
  ~:*~A /remotecontrol/stop(\"now\")
"
	    ;; (progname)
	    "rsb-call")))

(defun update-synopsis (&key
			(show :default))
  "Create and return a commandline option tree."
  (make-synopsis
   :postfix "SERVER-URI/METHOD(ARG)"
   :item    (make-text :contents (make-help-string))
   :item    (make-common-options :show show)
   ;; Append RSB options.
   :item    (make-options
	     :show? (or (eq show t)
			(and (listp show) (member :rsb show))))))

(defun parse-argument (string)
  "Parse STRING as Lisp object treating the empty string specially."
  (if (emptyp string)
      rsb.converter:+no-value+
      (read-from-string string)))

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
		("([a-z0-9/:&=;]+)/([a-z0-9]+)\\((.*)\\)" spec)
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

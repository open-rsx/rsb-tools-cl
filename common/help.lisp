;;; help.lisp --- Automatic generation of help strings.
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

(in-package :rsb.common)


;;; URI synopsis for connectors
;;

(defun print-uri-synopsis (connector-class stream)
  "Return a synopsis string for the URI syntax of CONNECTOR-CLASS."
  (bind (((:accessors-r/o
	   (schema  rsb.transport:connector-schemas)
	   (options rsb.transport:connector-options)) connector-class)
	 (host  (find :host options :key #'first))
	 (port  (find :port options :key #'first))
	 (other (remove-if #'(lambda (option)
			       (member (first option) '(:host :port)))
			   options)))
    (format stream
	    "~(~A~):~:[~;[//HOST]~]~:[~;[:PORT]~]/SCOPE~@[[?~:{~(~A~)=A-~A~^;~}]~]"
	    (first schema) host port other)))

(defun print-all-uri-synopsis (stream)
  "Print synopsis strings for all known transport implementation on
STREAM."
  (pprint-logical-block (stream nil)
    (iter (for (name class) in (remove-duplicates
				(rsb.transport:transport-classes)
				:key  (compose #'rsb.transport:connector-schemas
					       #'second)
				:test #'equal))
	  (print-uri-synopsis class stream)
	  (pprint-newline :mandatory stream))))


;;; URI help string
;;

(defun print-uri-help-string (stream)
  "Print an explanatory string regarding the interpretation of URIS
onto STREAM."
  (format stream
	  "SCHEME:[//HOST][:PORT][/PATH][?QUERY][#FRAGMENT]

is interpreted as follows:

+ SCHEME   -> Transport name
+ HOST     -> Transport option :HOST (not always supported)
+ PORT     -> Transport option :PORT (not always supported)
+ PATH     -> Scope
+ QUERY    -> \"freestyle\" transport options. Has to be of the form ~
KEY1=VALUE1;KEY2=VALUE2;...
+ FRAGMENT -> not processed

For the currently available transports, URIs should match the ~
following patterns:

~2T")
  (print-all-uri-synopsis stream))


;;; Version string
;;

(defun print-version (version stream
		      &key
		      include-rsb-version?)
  "Format VERSION onto STREAM as a version number. If
INCLUDE-RSB-VERSION? is non-nil, additionally format the RSB version
onto STREAM."
  (format stream "~A version ~{~D.~D.~D~}" (progname) version)
  (when include-rsb-version?
    (format stream "~&RSB Version ~{~D.~D.~D~}"
	    (cl-rsb-system:version/list))))

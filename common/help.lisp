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

(defun print-uri-help (stream)
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


;;; Filter help string
;;

(defun print-filter-help (stream
			  &key
			  (blacklist '(:or :disjoin :and :conjoin)))
  "Format a table of filter names and corresponding documentation
strings onto STREAM."
  (bind ((items (remove-duplicates
		 (remove-if (rcurry #'member blacklist)
			    (rsb.filter:filter-classes)
			    :key #'first)
		 :key #'second))
	 ((:flet do-one (name class))
	  (list name (documentation (class-name class) 'type))))
    (format stream "~{~{~(~A~) ARGS~&~2T~@<~@;~A~:>~}~^~&~}"
	    (map 'list (curry #'apply #'do-one) items))))


;;; Version string
;;

(defun print-version (version stream
		      &key
		      (include-lisp-version? t)
		      (include-rsb-version?  t)
		      more-versions)
  "Format VERSION onto STREAM as a version number.
If INCLUDE-LISP-VERSION? is non-nil, additionally format the Lisp
implementation name and version onto STREAM.
If INCLUDE-RSB-VERSION? is non-nil, additionally format the RSB
version onto STREAM.
MORE-VERSIONS is a \"plist\" of additional component names and
associated versions that should be printed onto STREAM."
  (bind (;; Compute the list of all requested versions.
	 (versions
	  (append `((,(progname) ,version))
		  (when include-lisp-version?
		    `((,(lisp-implementation-type)
			,(lisp-implementation-version))))
		  (when include-rsb-version?
		    `(("RSB" ,(cl-rsb-system:version/list))))
		  (iter (for (name version) on more-versions :by #'cddr)
			(collect (list (string name) version)))))
	 ;; Compute the common column for all versions to be printed
	 ;; to.
	 (version-column (+ (reduce #'max versions
				    :key (compose #'length #'first))
			    1
			    (length "version")
			    1))
	 ((:flet format-version (info))
	  (bind (((name version) info))
	    (format stream "~A version~VT~:[~{~D.~D.~D~}~;~A~]~&"
		    name version-column (stringp version) version))))
    (map nil #'format-version versions)))

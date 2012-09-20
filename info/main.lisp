;;; main.lisp --- Entry point of the info tool.
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

(cl:in-package :rsb.tools.info)

(defun update-synopsis (&key
			(show :default))
  "Create and return a commandline option tree."
  (make-synopsis
   ;; Basic usage and specific options.
   :item    (make-text :contents "Describe the RSB system.")
   :item    (make-common-options :show show)
   ;;
   :item    (defgroup (:header "Display Options")
	      (switch :long-name       "verbose"
		      :short-name      "b"
		      :default-value   nil
		      :description
		      "Display all available information.")
	      (switch :long-name       "configuration"
		      :short-name      "f"
		      :default-value   nil
		      :description
		      "Display information regarding the default configuration?")
	      (switch :long-name       "connectors"
		      :short-name      "c"
		      :default-value   nil
		      :description
		      "Display information regarding available transport implementations?")
	      (switch :long-name       "converters"
		      :short-name      "v"
		      :default-value   nil
		      :description
		      "Display information regarding available converters?")
	      (switch :long-name       "filters"
		      :short-name      "i"
		      :default-value   nil
		      :description
		      "Display information regarding available filters?")
	      (switch :long-name       "event-processing"
		      :short-name      "e"
		      :default-value   nil
		      :description
		      "Display information regarding available event processing strategies?"))
   ;; Append RSB options.
   :item    (make-options
	     :show? (or (eq show t)
			(and (listp show) (member :rsb show))))))

(defun main ()
  "Entry point function of the cl-rsb-tools-info system."
  (update-synopsis)
  (setf *default-configuration* (options-from-default-sources))
  (process-commandline-options
   :version         (cl-rsb-tools-info-system:version/list :commit? t)
   :update-synopsis #'update-synopsis
   :return          #'(lambda () (return-from main)))
  (enable-swank-on-signal)

  (let+ ((stream   *standard-output*)
	 (verbose? (getopt :long-name "verbose"))
	 ((version? configuration? connectors? converters? filters?
	   event-processing?)
	  (mapcar #'(lambda (name)
		      (or (getopt :long-name name) verbose?))
		  '("version" "configuration" "connectors" "converters"
		    "filters" "event-processing"))))
    (with-print-limits (stream)
      (with-logged-warnings
	(when version?
	  (print-version nil stream))

	(when configuration?
	  (rsb.formatting::with-indented-section (stream "Configuration")
	    (format stream "~{~48@<~(~{~A~^.~}~)~>: ~S~^~&~}"
		    (alist-plist *default-configuration*))))

	(when connectors?
	  (rsb.formatting::with-indented-section (stream "Connectors")
	    (format stream
		    "~{+ ~<~@;~@{~A~*~}~:>~^~&~}"
		    (rsb.transport:transport-classes))))

	(when converters?
	  (rsb.formatting::with-indented-section (stream "Converters")
	   (format stream
		   "~{+ ~<~@;~@{~A~*~}~:>~^~&~}"
		   (rsb.converter:converter-classes))))

	(when filters?
	  (rsb.formatting::with-indented-section (stream "Filters")
	    (print-filter-help stream)))

	(when event-processing?
	  (format stream
		  "~%Event Processors~%~{+ ~<~@;~@{~A~*~}~:>~^~&~}~%"
		  (rsb.event-processing:processor-classes)))))))

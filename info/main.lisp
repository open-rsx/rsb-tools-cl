;;; main.lisp --- Entry point of the info tool.
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
	      (switch :long-name       "connectors"
		      :short-name      "c"
		      :default-value   nil
		      :description
		      "Display information regarding available transport implementations?")
	      (switch :long-name       "converters"
		      :short-name      "v"
		      :default-value   nil
		      :description
		      "Display information regarding available converters?"))
   ;; Append RSB options.
   :item    (make-options
	     :show? (or (eq show t)
			(and (listp show) (member :rsb show))))))

(defun main ()
  "Entry point function of the cl-rsb-tools-info system."
  (update-synopsis)
  (setf *default-configuration* (options-from-default-sources))
  (process-commandline-options
   :version         (cl-rsb-tools-info-system:version/list)
   :update-synopsis #'update-synopsis
   :return          #'(lambda () (return-from main)))
  (enable-swank-on-signal)

  (with-print-limits (*standard-output*)
    (with-logged-warnings
      (let ((connectors? (getopt :long-name "connectors"))
	    (converters? (getopt :long-name "converters")))
	(print-version nil *standard-output*)

	(when connectors?
	  (format *standard-output*
		  "~%Connectors~%~{+ ~<~@;~@{~A~*~}~:>~^~&~}~%"
		  (rsb.transport:transport-classes)))

	(when converters?
	  (format *standard-output*
		  "~%Converters~%~{+ ~<~@;~@{~A~*~}~:>~^~&~}~%"
		  (rsb.converter:converter-classes)))))))

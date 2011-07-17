;;; protocol.lisp --- Protocol for formatting of RSB events.
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

(in-package :rsb.formatting)


;;; Event formatting protocol
;;

(defgeneric format-event (event style stream
			  &key
			  max-lines
			  max-columns)
  (:documentation
   "Format EVENT onto STREAM using a style designated by STYLE.
MAX-LINES controls specifies the maximum number of lines the produced
output is allowed to take up.
MAX-COLUMNS limits the number of columns individual output lines are
allowed to take up."))

(defgeneric format-payload (data style stream
			    &key
			    max-lines
			    max-columns)
  (:documentation
   "Format the event payload DATA onto STREAM using a formatting style
designated by STYLE.
MAX-LINES controls specifies the maximum number of lines the produced
output is allowed to take up.
MAX-COLUMNS limits the number of columns individual output lines are
allowed to take up."))


;;;
;;

(defun format-styles (format-function)
  "Return a list of items that are styles and descriptions of the
form (STYLE DESCRIPTION)."
  (bind (((:flet method-style (method))
	  (let ((style-specializer (second (closer-mop:method-specializers method))))
	    (when (typep style-specializer 'closer-mop:eql-specializer)
	      (closer-mop:eql-specializer-object style-specializer))))
	 (methods (remove-if-not #'method-style
				 (closer-mop:generic-function-methods
				  (fdefinition format-function)))))
    (remove-duplicates
     (map 'list #'(lambda (method)
		    (list (method-style method) (documentation method t)))
	  methods)
     :key #'first)))

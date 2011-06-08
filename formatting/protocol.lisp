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

(defgeneric format-event (event style stream)
  (:documentation
   "Format EVENT onto STREAM using a style designated by STYLE."))

(defgeneric format-payload (data style stream)
  (:documentation
   "Format the event payload DATA onto STREAM using a formatting style
designated by STYLE."))


;;;
;;

(defun format-styles (format-function)
  "TODO(jmoringe): document"
  (bind (((:flet method-style (method))
	  (let ((style-specializer (second (closer-mop:method-specializers method))))
	    (when (typep style-specializer 'closer-mop:eql-specializer)
	      (closer-mop:eql-specializer-object style-specializer))))
	 (methods (closer-mop:generic-function-methods
		   (fdefinition format-function)))
	 (all-styles (remove nil (map 'list #'method-style methods))))
    (remove-duplicates all-styles)))

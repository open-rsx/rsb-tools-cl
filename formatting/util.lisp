;;; util.lisp --- Utility functions for event formatting.
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

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defmacro with-indent ((stream-var
			  &key
			  (amount              2)
			  (initial-fresh-line? t)
			  (final-fresh-line?   t))
			 &body body)
    "Execute BODY with the stream of the current emit target bound to a
pretty-printing stream that indents all output produced within BODY to
a certain depth. In addition, a scope of kind KIND and name NAME is
printed around the output."
    `(progn
       ,@(when initial-fresh-line?
	       `((format ,stream-var "~&")))
       (pprint-logical-block (,stream-var nil
					  :per-line-prefix ,(make-string amount :initial-element #\Space))
	 ,@body)
       ,@(when final-fresh-line?
	       `((format ,stream-var "~&")))))

  (defmacro with-indented-section ((stream-var title
				    &key
				    (amount            2)
				    (final-fresh-line? t))
				   &body body)
    "Execute BODY with STREAM-VAR bound to a stream that indents all
content by AMOUNT."
    `(progn
       ,@(when title
	       `((format ,stream-var (format nil "~A~~&" ,title))))
       (with-indent (,stream-var :amount              ,amount
				 :initial-fresh-line? nil
				 :final-fresh-line?   ,final-fresh-line?)
	 ,@body))))

(defun format-aligned-items (stream keys values
			     &key
			     (value-formatter #'format-maybe))
  "Format KEYS and VALUES onto STREAM such that keys and values align
vertically across output lines."
  (bind ((width  (reduce #'max keys
			 :key           (compose #'length #'string)
			 :initial-value 0)))
    (iter (for key   in keys)
	  (for value in values)
	  (unless (first-iteration-p)
	    (format stream "~&"))
	  (format stream "~:(~VA~): " width key)
	  (funcall value-formatter stream value))))

(defun format-aligned-items/alist (stream items
				   &key
				   value-formatter)
  "TODO(jmoringe): document"
  (format-aligned-items
   stream (map 'list #'car items) (map 'list #'cdr items))
  :value-formatter value-formatter)

(declaim (special *tracker*))

(defvar *tracker* nil
  "This variable should be dynamically bound to a hash-table which is
then used by `format-recursively' to detect already formatted
objects.")

(defun format-recursively (stream value
			   &key
			   (tracker (or *tracker* (make-hash-table))))
  "TODO(jmoringe): document"
  (let ((*tracker* tracker))
    (if (gethash value tracker)
	(format stream "~A" value)
	(progn
	  (setf (gethash value tracker) t)
	  (etypecase value
	    (string
	     (format stream "~S" value))
	    (sequence
	     (if (emptyp value)
		 (format stream "<empty sequence>")
		 (with-indent (stream :final-fresh-line? nil)
		   (iter (for item each value)
			 (unless (first-iteration-p)
			   (format stream "~&"))
			 (format-recursively stream item)))))
	    (standard-object
	     (format-instance stream value))
	    (t
	     (format stream "~A" value)))))))

(defun format-instance (stream instance)
  "Format INSTANCE onto STREAM, handling slot values recursively."
  (bind (((:flet slot-value* (name))
	  (if (slot-boundp instance name)
	      (slot-value instance name)
	      "UNBOUND"))
	 (keys   (map 'list #'closer-mop:slot-definition-name
		      (closer-mop:class-slots (class-of instance))))
	 (values (map 'list #'slot-value* keys)))
    (format stream "~A" instance)
    (with-indent (stream :final-fresh-line? nil)
      (format-aligned-items stream keys values
			    :value-formatter #'format-recursively))))

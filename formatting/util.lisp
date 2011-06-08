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

  (defmacro with-indent ((stream-var) &body body)
    "Execute BODY with the stream of the current emit target bound to a
pretty-printing stream that indents all output produced within BODY to
a certain depth. In addition, a scope of kind KIND and name NAME is
printed around the output."
    `(progn
       (pprint-logical-block (,stream-var nil
					  :per-line-prefix "  ")
	 ,@body)))

  (defmacro with-indented-section ((stream-var &optional title)
				   &body body)
    "TODO(jmoringe): document"
    `(progn
       ,@(when title
	       `((format ,stream-var (format nil "~A~~&" ,title))))
       (with-indent (,stream-var)
	 ,@body))))

;; TODO make a function like

(defun format-aligned-items (stream keys values
			     &key
			     (value-formatter #'format-maybe))
  "TODO(jmoringe): document"
  (bind ((width  (reduce #'max keys
			 :key (compose #'length #'string)))
	 (format (format nil "~~(~~~DA~~): " width)))
    (iter (for key   in keys)
	  (for value in values)
	  (format stream format key)
	  (funcall value-formatter stream value)
	  (format stream "~&"))))

(defun format-aligned-items/alist (stream items
				   &key
				   value-formatter)
  "TODO(jmoringe): document"
  (format-aligned-items
   stream (map 'list #'car items) (map 'list #'cdr items))
  :value-formatter value-formatter)

(defun format-maybe (stream value)
  "TODO(jmoringe): document"
  (format stream "~:[N/A~;~:*~A~]" value))

(defun format-recursively (stream value)
  "TODO(jmoringe): document"
  (etypecase value
    (null
     (format stream "N/A"))
    (standard-object
     (format stream "~A~&" value)
     (format-instance stream value))
    (t
     (format stream "~A" value))))

(defun format-instance (stream instance)
  "TODO(jmoringe): document"
  (bind (((:flet slot-value* (name))
	  (if (slot-boundp instance name)
	      (slot-value instance name)
	      "UNBOUND"))
	 (keys   (map 'list #'closer-mop:slot-definition-name
		      (closer-mop:class-direct-slots (class-of instance))))
	 (values (map 'list #'slot-value* keys)))
    (with-indent (stream)
      (format-aligned-items stream keys values
			    :value-formatter #'format-recursively))))

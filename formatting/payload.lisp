;;; payload.lisp --- Formatting methods for different kinds of event payloads.
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

(defmethod format-payload :around ((payload t) (style t) (stream t)
				   &key
				   (max-lines   4)
				   (max-columns 80)) ;;; TODO(jmoringe): default from clon?
  "Supply payload-independent defaults for MAX-LINES and MAX-COLUMNS."
  (call-next-method payload style stream
		    :max-lines   max-lines
		    :max-columns max-columns))

(defmethod format-payload ((payload t) (style t) (stream t)
			   &key
			   max-lines
			   max-columns)
  "Default behavior is to print PAYLOAD using the Lisp printer."
  (let ((*print-miser-width*  (floor max-columns 2))
	(*print-right-margin* max-columns))
    (format stream "~A" payload)))

(defmethod format-payload ((payload vector) (style t) (stream t)
			   &key
			   max-lines
			   max-columns)
  "Format PAYLOAD in form of a hexdump if it is an octet-vector."
  (if (typep payload 'octet-vector)
      (iter (for  byte   in-vector payload)
	    (for  offset :from     0)
	    (with line   =         1)
	    (with column =         1)
	    (when (or (= line max-lines) (= offset 400))
	      (format stream ".... ..")
	      (terminate))
	    (when (= column 1)
	      (format stream "~4,'0X" offset))
	    (format stream " ~2,'0X" byte)
	    (when (= (incf column) (1+ (floor (- max-columns 4) 3)))
	      (format stream "~&")
	      (incf line)
	      (setf column 1)))
      (call-next-method)))

(defmethod format-payload ((payload string) (style t) (stream t)
			   &key
			   max-lines
			   max-columns)
  "Format PAYLOAD as a multi-line string, trying to honor MAX-LINES
and MAX-COLUMNS constraints."
  (iter (for  c    in-vector payload)
	(for  size :from 0)
	(with line =     1)
	(with column =   1)
	(when (or (= line max-lines) (= size 400))
	  (format stream "...")
	  (terminate))
	(if (or (eq c #\Newline) (= column max-columns))
	    (progn
	      (unless (eq c #\Newline)
		(format stream "\\"))
	      (fresh-line stream)
	      (incf line)
	      (setf column 1))
	    (progn
	      (write-char c stream)
	      (incf column)))))

(defmethod format-payload ((payload standard-object) (style t) (stream t)
			   &key
			   max-lines
			   max-columns)
  "Recursively format the slot values of PAYLOAD."
  (format-instance stream payload))

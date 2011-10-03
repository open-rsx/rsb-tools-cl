;;; format-functions.lisp --- RSB-specific formatting functions.
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


;;; Utility functions
;;

(declaim (inline length-exhausted? columns-exhausted? lines-exhausted?))

(defun length-exhausted? (count)
  (and *print-length* (>= count *print-length*)))

(defun columns-exhausted? (column)
  (and *print-right-margin* (>= column *print-right-margin*)))

(defun lines-exhausted? (line)
  (and *print-lines* (>= line *print-lines*)))

(defun ascii-character-code? (code)
  "Return non-nil if the character associated to CODE is both
printable and in the ASCII character set."
  (or (member code '(8 10 13) :test #'=)
      (<= 32 code 127)))


;;; Print functions
;;

(defun format-octet-vector (stream value &optional
			    (colon? nil colon-supplied?)
			    at?)
  "Format the octet vector VALUE onto STREAM using hex-dump format.
COLON? controls whether offsets are printing at the beginnings of all
lines."
  (declare (ignore at?))

  (let* ((length         (length value))
	 (print-offsets? (if colon-supplied?
			     colon?
			     (columns-exhausted? (* length 3)))))
    (iter (for  byte   in-vector value :with-index offset)
	  (with line   =         1)
	  (with column =         1)
	  ;; Terminate?
	  (when (or (and (lines-exhausted? line)
			 (columns-exhausted? (* (- length offset) 3)))
		    (length-exhausted? offset))
	    (format stream "... [omitting ~:D of ~:D octets]"
		    (- length offset) length)
	    (terminate))
	  ;; Maybe print offset.
	  (when (and print-offsets? (= column 1))
	    (format stream "~4,'0X " offset)
	    (incf column 5))
	  ;; Print byte.
	  (format stream "~2,'0X " byte)
	  (when (columns-exhausted? (incf column 3))
	    (fresh-line stream)
	    (incf line)
	    (setf column 1))
	  ;; Print as string, if possible.
	  (finally
	   (when (and (not (lines-exhausted? line))
		      (every #'ascii-character-code? value))
	     (let ((as-string (sb-ext:octets-to-string value)))
	       (when (or (> line 1)
			 (columns-exhausted? (+ column (length as-string) 4)))
		 (fresh-line stream)
		 (incf line)
		 (setf column 1))
	       (format stream "(~/rsb.formatting::format-string/)"
		       as-string)))))))

(defun format-string (stream value &optional colon? at?)
  "Format the string VALUE onto STREAM."
  (declare (ignore colon? at?))

  (format stream "\"")
  (iter (for  c      in-vector value :with-index size)
	(with line   =         1)
	(with column =         2)
	;; Terminate?
	(when (or (lines-exhausted? (1- line))
		  (and (lines-exhausted? line)
		       (columns-exhausted? (+ column 3 (- 1))))
		  (length-exhausted? size))
	  (format stream "...")
	  (terminate))
	;; Print character.
	(cond
	  ((or (eq c #\Newline) (columns-exhausted? column))
	   (unless (eq c #\Newline)
	     (format stream "\\"))
	   (fresh-line stream)
	   (incf line)
	   (setf column 1))
	  (t
	   (write-char c stream)
	   (incf column))))
  (format stream "\""))

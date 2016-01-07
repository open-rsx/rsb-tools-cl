;;;; format-functions.lisp --- RSB-specific formatting functions.
;;;;
;;;; Copyright (C) 2011, 2013, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.formatting)

;;; Utility functions

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

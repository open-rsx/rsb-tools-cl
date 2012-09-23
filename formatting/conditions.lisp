;;; conditions.lisp --- Conditions used in the formatting module.
;;
;; Copyright (C) 2012 Jan Moringen
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

(cl:in-package :rsb.formatting)

(define-condition format-code-error (error)
  ((code :initarg  :code
	 :reader   format-code-error-code
	 :documentation
	 "Stores the user-supplied format code that caused the
problem."))
  (:report
   (lambda (condition stream)
     (format stream "~@<Failed to use format code ~S.~@:>"
	     (format-code-error-code condition))))
  (:documentation
   "This error is signaled when an error related to user-supplied
format code occurs."))

(define-condition simple-format-code-error (format-code-error
					    simple-error)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<Failed to use format code ~S~@:>"
	     (format-code-error-code condition))
     (maybe-print-explanation stream condition)))
  (:documentation
   "This `simple-error' is signaled when an error related to
user-supplied format code occurs."))

(defun format-code-error (code format-control &rest format-arguments)
  "Signal a `simple-format-code-error' with description FORMAT-CONTROL
and FORMAT-ARGUMENTS when user-supplied CODE caused a problem."
  (error 'simple-format-code-error
	 :code             code
	 :format-control   format-control
	 :format-arguments format-arguments))

(define-condition format-code-read-error (format-code-error
					  chainable-condition)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<Failed to read formatting code from ~S~@:>."
	     (format-code-error-code condition))
     (maybe-print-cause stream condition)))
  (:documentation
   "This error is signaled when reading user-supplied format code
fails."))

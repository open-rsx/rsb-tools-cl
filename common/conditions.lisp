;;; conditions.lisp --- Conditions used in the cl-rsb-common system.
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

(in-package :rsb.common)

(define-condition failed-to-load-idl (rsb-error
				      chainable-condition)
  ((source :initarg  :source
	   :reader   failed-to-load-idl-source
	   :documentation
	   ""))
  (:report
   (lambda (condition stream)
     (format stream "~@<Failed to load data definition from source ~
~A.~/rsb::maybe-print-cause/~@:>"
	     (failed-to-load-idl-source condition)
	     (chainable-condition-cause condition))))
  (:documentation
   "This error is signaled when an attempt to load a data definition
from some source fails."))

(defun failed-to-load-idl (source &optional cause)
  "Convenience function for signaling `failed-to-load-idl'."
  (apply #'error 'failed-to-load-idl
	 :source source
	 (when cause
	   (list :cause cause))))

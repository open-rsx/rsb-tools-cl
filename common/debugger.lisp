;;; debugger.lisp --- Disabling the debugger.
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

(defun trace-things (specs)
  "Like `trace', but SPECS is evaluated."
  #+sbcl (eval (sb-debug::expand-trace specs))
  #-sbcl (error "Not implemented"))

(defun disable-debugger ()
  "Disable the debugger and return."
  #+sbcl (setf sb-ext:*invoke-debugger-hook*
	       (lambda (condition previous-value)
		 (declare (ignore previous-value))
		 (format *error-output* "~A~%" condition)
		 (com.dvlsoft.clon:exit 1))))

(defun start-swank ()
  "Start a swank server and write its port to \"./swank-port.txt\"."
  (ql:quickload :swank)
  (funcall (find-symbol "START-SERVER" :swank) "./swank-port.txt"))
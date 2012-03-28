;;; interactive.lisp --- Functions for interactive stuff.
;;
;; Copyright (C) 2011, 2012 Jan Moringen
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

(cl:in-package :rsb.common)

(defmacro with-interactive-interrupt-exit ((&key
					    (signals '(sb-posix:SIGINT
						       sb-posix:SIGTERM)))
					   &body body)
  "Run BODY with an interruption handler that exits non-locally and
returns nil instead of entering the debugger."
  `(catch 'terminate
     ,@(iter (for signal in signals)
	     #-win32
	     (collect
		 `(sb-unix::enable-interrupt
		   ,signal
		   #'(lambda (signal info context)
		       (declare (ignore signal info context))
		       (throw 'terminate nil)))) )
     ,@body))

(defun interrupt (&optional thread)
  "Cause a `with-interactive-interrupt-exit' form to interrupt its
body, clean up and return.
If supplied, THREAD specifies the thread executing the form."
  (if thread
      (bt:interrupt-thread thread
			   #'(lambda ()
			       (sleep 1)
			       (throw 'terminate nil)))
      (throw 'terminate nil)))

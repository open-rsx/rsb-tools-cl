;;; error-handling.lisp --- Toplevel error handling functions.
;;
;; Copyright (C) 2012 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This file may be licensed under the terms of the
;; GNU Lesser General Public License Version 3 (the ``LGPL''),
;; or (at your option) any later version.
;;
;; Software distributed under the License is distributed
;; on an ``AS IS'' basis, WITHOUT WARRANTY OF ANY KIND, either
;; express or implied. See the LGPL for the specific language
;; governing rights and limitations.
;;
;; You should have received a copy of the LGPL along with this
;; program. If not, go to http://www.gnu.org/licenses/lgpl.html
;; or write to the Free Software Foundation, Inc.,
;; 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
;;
;; The development of this software was supported by:
;;   CoR-Lab, Research Institute for Cognition and Robotics
;;     Bielefeld University

(cl:in-package :rsb.common)


;;; Toplevel error handling strategies
;;

(defun abort/signal (&optional condition)
  "Like `cl:abort' but signal CONDITION using the `abort/signal'
RESTART.

This function is intended to be used as toplevel error policy."
  (if-let ((abort/signal (find-restart 'abort/signal condition)))
    (invoke-restart 'abort/signal condition)
    (abort)))

(defun continue/verbose (&optional condition)
  "Like `cl:continue' but log a warning if `cl:continue' restart is
established and a different warning if it does not.

This function is intended to be used as toplevel error policy."
  (if-let ((restart (find-restart 'continue condition)))
    (progn
      (log1 :warn "~@<Error encountered; Recovering via restart~
~2&~<| ~@;~S: ~A~:>~
~2&. Further processing may yield partial or incorrect results.~@[ Error was:~
~2&~<| ~@;~A~:>~
~2&.~]~:>"
	    (list (restart-name restart) restart)
	    (when condition (list condition)))
      (invoke-restart restart))
    (log1 :warn "~@<No ~S restart; Cannot recover~@[. Error:~
~2&~<| ~@;~A~:>~
~2&.~]~:>"
	  'continue (when condition (list condition)))))


;;; Toplevel utility functions and macros
;;

(defun maybe-relay-to-thread (strategy
			      &key
			      (target-thread (bt:current-thread)))
  "Return a function of one argument which executes STRATEGY but
establishes a TRANSFER-ERROR restart unless executing in
TARGET-THREAD. Invoking the TRANSFER-ERROR restart does not perform a
non-local control transfer. That is, STRATEGY still has to handle the
condition after invoking the restart.

STRATEGY has to accept a condition as its sole argument."
  #'(lambda (condition)
      (let ((thread (bt:current-thread)))
	(if (eq thread target-thread)

	    ;; When executing in TARGET-THREAD, the error cannot be
	    ;; transferred.
	   (restart-bind
	       ((abort #'(lambda (condition))))
	     (funcall strategy condition))

	   ;; When executing in a different thread, the error can be
	   ;; transferred to TARGET-THREAD.
	   (progn
	     (log1 :info "Applying error policy ~A in background thread ~A"
		   strategy thread)
	     (restart-case
		 (funcall strategy condition)
	       (abort (&optional condition)
		 (log1 :info "Error policy ~A aborted in background thread ~A. Aborting in main thread."
		       strategy thread)
		 (bt:interrupt-thread
		  target-thread #'(lambda ()
				    (invoke-restart 'abort/signal condition)))))
	     (log1 :warn "Aborting background thread ~A" thread)
	     (abort))))))

(declaim (ftype (function (function function) *)
		invoke-with-error-handling-strategy))

(defun invoke-with-error-policy (policy thunk)
  "Call THUNK with `cl:abort' and `abort/signal' restarts
established. When an error is signaled, call POLICY to (potentially)
handle it."
  (restart-bind
      ((abort        #'(lambda ()
			 (error "Aborted.")))
       (abort/signal #'(lambda (condition)
			 (error condition))))
    (handler-bind
	((error policy))
      (funcall thunk))))

(defmacro with-error-policy ((policy) &body body)
  "Execute BODY with `cl:abort' and `abort/signal' restarts
established. When an error is signaled, call POLICY to (potentially)
handle it."
  `(invoke-with-error-policy
    (coerce ,policy 'function) #'(lambda () ,@body)))

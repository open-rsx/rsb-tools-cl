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

(defun abort/signal (condition)
  "Like `cl:abort' but signal CONDITION using the `abort/signal'
RESTART.

This function is intended to be used as toplevel error policy."
  (if-let ((abort/signal (find-restart 'abort/signal condition)))
    (invoke-restart abort/signal condition)
    (abort condition)))

(defun continue/verbose (&optional condition)
  "Like `cl:continue' but log a warning if `cl:continue' restart is
established and a different warning if it does not.

This function is intended to be used as toplevel error policy."
  (if-let ((restart (find-restart 'continue condition)))
    (progn
      (log1 :warn "Error encountered; Recovering via restart~
~2&~<| ~@;~S: ~A~:>~
~2&. Further processing may yield partial or incorrect results.~@[ Error was:~
~2&~<| ~@;~A~:>~
~2&.~]"
	    (list (restart-name restart) restart)
	    (when condition (list condition)))
      (invoke-restart restart))
    (log1 :warn "No ~S restart; Cannot recover~@[. Error:~
~2&~<| ~@;~A~:>~
~2&.~]"
	  'continue (when condition (list condition)))))


;;; Toplevel utility functions and macros
;;

(defun maybe-relay-to-thread (policy
			      &key
			      (target-thread (bt:current-thread)))
  "Return a function of one argument which executes POLICY but
establishes a TRANSFER-ERROR restart unless executing in
TARGET-THREAD. Invoking the TRANSFER-ERROR restart does not perform a
non-local control transfer. That is, POLICY still has to handle the
condition after invoking the restart.

POLICY has to accept a condition as its sole argument.

TARGET-THREAD has to be a `bt:thread' object and defaults
to (bt:current-thread)."
  #'(lambda (condition)
      (let ((thread (bt:current-thread)))
	(if (eq thread target-thread)

	    ;; When executing in TARGET-THREAD, the error cannot be
	    ;; transferred. Thus, we install no restarts.
	    ;;
	    ;; When POLICY is `abort/signal', we can just unwind,
	    ;; preserving the backtrace.
	   (if (eq policy #'abort/signal)
	       (log1 :info "Error policy ~S in thread ~A; unwinding normally."
		     policy thread)
	       (progn
		 (funcall policy condition)
		 ;; If POLICY did not handle CONDITION, abort
		 ;; TARGET-THREAD, signaling CONDITION.
		 (invoke-restart 'abort/signal condition)))

	   ;; When executing in a different thread, the error can be
	   ;; transferred to TARGET-THREAD.
	   (progn
	     (log1 :info "Applying error policy ~A in background thread ~A"
		   policy thread)
	     (restart-case
		 (funcall policy condition)
	       (abort (&optional condition)
		 (declare (ignore condition))
		 (bt:interrupt-thread
		  target-thread #'(lambda () (invoke-restart 'abort))))
	       (abort/signal (condition)
		 (log1 :warn "Error policy ~A aborted with condition ~
in background thread ~A. Aborting with condition in main thread. ~
Condition was:~
~2&~<| ~@;~A~:>~
~2&."
		       policy thread (list condition))

		 (bt:interrupt-thread
		  target-thread #'
		  (lambda ()
		    (invoke-restart 'abort/signal condition)))))
	     (log1 :info "Aborting background thread ~A" thread)
	     (abort))))))

(declaim (ftype (function (function function) *)
		invoke-with-error-policy))

(defun invoke-with-error-policy (policy thunk)
  "Call THUNK with `cl:abort' and `abort/signal' restarts
established. When an error is signaled, call POLICY to (potentially)
handle it."
  (restart-case
      (handler-bind
	  ((error policy))
	(funcall thunk))
    (abort ()
      (error "Aborted."))
    (abort/signal (condition)
      (error condition))))

(defmacro with-error-policy ((policy) &body body)
  "Execute BODY with `cl:abort' and `abort/signal' restarts
established. When an error is signaled, call POLICY to (potentially)
handle it."
  `(invoke-with-error-policy
    (coerce ,policy 'function) #'(lambda () ,@body)))

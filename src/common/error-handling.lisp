;;;; error-handling.lisp --- Toplevel error handling functions.
;;;;
;;;; Copyright (C) 2012, 2013, 2014, 2016 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;;
;;;; This file may be licensed under the terms of the

(cl:in-package #:rsb.tools.common)

;;; Toplevel error handling strategies

(defun abort/signal (condition)
  "Like `cl:abort' but signal CONDITION using the `abort/signal'
   RESTART.

   This function is intended to be used as toplevel error policy."
  (if-let ((abort/signal (find-restart 'abort/signal condition)))
    (invoke-restart abort/signal condition)
    (abort condition)))

(defun continue/verbose (&optional condition)
  "Like `cl:continue' but log a warning if `cl:continue' restart is
   established and a different warning if it is not.

   This function is intended to be used as toplevel error policy."
  (if-let ((restart (find-restart 'continue condition)))
    (progn
      (log:warn "Error encountered; Recovering via restart ~
                 ~:@_~:@_~<| ~@;~S: ~A~:>~
                 ~:@_~:@_. Further processing may yield partial or ~
                 incorrect results.~@[ Error was:~
                 ~:@_~:@_~<| ~@;~A~:>~
                 ~:@_~:@_.~]"
                (list (restart-name restart) restart)
                (when condition (list condition)))
      (invoke-restart restart))
    (log:warn "No ~S restart; Cannot recover~@[. Error:~
               ~:@_~:@_~<| ~@;~A~:>~
               ~:@_~:@_.~]"
              'continue (when condition (list condition)))))

;;; Toplevel utility functions and macros

(defun maybe-relay-to-thread (policy
                              &key
                              (target-thread (bt:current-thread)))
  "Return a function of one argument which executes POLICY but
   establishes a TRANSFER-ERROR restart unless executing in
   TARGET-THREAD. Invoking the TRANSFER-ERROR restart does not perform
   a non-local control transfer. That is, POLICY still has to handle
   the condition after invoking the restart.

   POLICY has to accept a condition as its sole argument.

   TARGET-THREAD has to be a `bt:thread' object and defaults to the
   value of (bt:current-thread) in the thread calling this function."
  (lambda (condition)
    (let ((thread (bt:current-thread)))
      (cond
        ;; When executing in a different thread, CONDITION can be
        ;; transferred to TARGET-THREAD. We establish restarts for
        ;; that.
        ((not (eq thread target-thread))
         (log:info "~@<Applying error policy ~A in background thread ~A~@:>"
                   policy thread)
         (restart-case
             (funcall policy condition)
           (abort (&optional condition)
             (declare (ignore condition))
             (bt:interrupt-thread target-thread #'abort))
           (abort/signal (condition)
             (log:warn "Error policy ~A aborted with condition in ~
                        background thread ~A. Aborting with condition ~
                        in main thread. Condition was:~
                        ~:@_~:@_~<| ~@;~A~:>~
                        ~:@_~:@_."
                       policy thread (list condition))

             (bt:interrupt-thread
              target-thread
              (lambda ()
                (invoke-restart 'abort/signal condition)))))
         ;; IF POLICY did not handle CONDITION or used one of our
         ;; restarts, we still have to abort the background thread.
         (log:info "~@<Aborting background thread ~A~@:>" thread)
         (abort))

        ;; When executing in TARGET-THREAD, CONDITION cannot be
        ;; transferred. Thus, we do not establish any restarts.

        ;; When POLICY is `abort/signal', we can just unwind,
        ;; preserving the backtrace.
        ((eq policy #'abort/signal)
         (log:info "~@<Error policy ~S in thread ~A; unwinding normally~@:>"
                   policy thread))

        ;; Otherwise, we give POLICY a chance to handle CONDITION.
        (t
         (funcall policy condition)
         ;; If POLICY did not handle CONDITION, just we can just
         ;; unwind preserving the backtrace (as above).
         )))))

(declaim (ftype (function (function function) *)
                call-with-error-policy))

(defun call-with-error-policy (policy thunk)
  "Call THUNK with `cl:abort' and `abort/signal' restarts
   established. When an error is signaled, call POLICY
   to (potentially) handle it."
  (restart-case
      (handler-bind ((error policy)) (funcall thunk))
    (abort (&optional condition)
      (declare (ignore condition))
      (error "Aborted."))
    (abort/signal (condition)
      (error condition))))

(defmacro with-error-policy ((policy) &body body)
  "Execute BODY with `cl:abort' and `abort/signal' restarts
   established. When an error is signaled, call POLICY
   to (potentially) handle it."
  `(call-with-error-policy
    (coerce ,policy 'function) (lambda () ,@body)))

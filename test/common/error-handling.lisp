;;;; error-handling.lisp --- Unit tests for error handling functions.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsb.common.test)

(define-condition foo-error (error) ()
  (:documentation
   "Mock condition class for error handling tests."))

(deftestsuite error-handling-root (common-root)
  ()
  (:documentation
   "Test suite for error-handling functions."))

(addtest (error-handling-root
          :documentation
	  "Smoke test for `abort', `abort/signal', `continue/verbose'
and `continue' error policies in combination with
`maybe-relay-to-thread' and `with-error-policy'.")
  policies/smoke

  (ensure-cases (error/signal policy abort?/expected error?/expected result/expected)
      `(;; Test all policies for the case in which an error condition
	;; is signaled from the *worker* thread.
	(:worker ,#'abort            t   nil :none)
	(:worker ,#'abort/signal     t   t   :none)
	(:worker ,#'continue/verbose nil nil :return/main)
	(:worker ,#'continue         nil nil :return/main)
	;; Test all policies for the case in which an error condition
	;; is signaled from the *main* thread.
	(:main   ,#'abort            t   nil :none)
	(:main   ,#'abort/signal     t   t   :none)
	(:main   ,#'continue/verbose nil nil :continue/main)
	(:main   ,#'continue         nil nil :continue/main)
	;; Test all policies for the case in which no conditions are
	;; signaled.
	(nil     ,#'abort            nil nil :return/main)
	(nil     ,#'abort/signal     nil nil :return/main)
	(nil     ,#'continue/verbose nil nil :return/main)
	(nil     ,#'continue         nil nil :return/main))

    (let+ ((policy/relay (maybe-relay-to-thread policy))
	   (abort?       t)
	   (result       :none)
	   ;; Mock client code which install a `continue' restart and
	   ;; signals an error, if requested.
	   ((&flet client-code (which result/normal result/continue)
	      (restart-case
		  (if (eq error/signal which)
		      (error 'foo-error)
		      result/normal)
		(continue (&optional condition)
		  (declare (ignore condition))
		  result/continue))))
	   ;; Mock of the typical structure of a worker thread:
	   ;; application of error policy, continue restart and
	   ;; "client code".
	   ((&flet spawn-worker-thread ()
	      (bt:make-thread
	       (lambda ()
		 (handler-bind ((error policy/relay))
		   (client-code :worker :return/worker :continue/worker))))))
	   ;; Mock of the typical structure of a main thread:
	   ;; `with-error-policy' and spawning of worker threads.
	   ((&flet do-it ()
	      (with-error-policy (policy/relay)
		;; Spawn worker thread and give it time to run,
		;; signal, interrupt us, etc.
		(ignore-errors (bt:join-thread (spawn-worker-thread)))
		(prog1
		    (client-code :main :return/main :continue/main)
		  (setf abort? nil))))))

      ;; Execute, expecting a condition, abort and a particular
      ;; result.
      (cond
	;; Expect main thread to be abort with `simple-error'
	;; "Aborted.".
	((and abort?/expected (not error?/expected))
	 (ensure-condition 'simple-error (do-it)))
	;; Expect main thread to signal `foo-error'.
	((and abort?/expected error?/expected)
	 (ensure-condition 'foo-error (do-it)))
	;; Expect main thread to return normally.
	(t
	 (setf result (do-it))))
      (ensure-same abort? abort?/expected)
      (ensure-same result result/expected))))

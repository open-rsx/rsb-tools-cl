;;;; interactive.lisp --- Functions for interactive stuff.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsb.common)

(defmacro with-interactive-interrupt-exit ((&key
                                            (signals       '(sb-posix:SIGINT
                                                             sb-posix:SIGTERM))
                                            (target-thread '(bt:current-thread)))
                                           &body body)
  "Run BODY with an interruption handler that exits non-locally and
returns nil instead of entering the debugger."
  ;; This whole macro is mainly needed for two reasons:
  ;; 1. Protect against multiple signals
  ;; 2. Perform proper shutdown for signals other than SIGINT,
  ;;    especially SIGTERM.
  (once-only (target-thread)
    `(restart-case
         (flet ((install-handler (signal)
                  (sb-unix::enable-interrupt
                   signal
                   (lambda (signal info context)
                     (declare (ignore info context))
                     ;; Install a handler which warns about the
                     ;; recursive signal and then ignores it.
                     (sb-unix::enable-interrupt
                      signal
                      (lambda (signal info context)
                        (declare (ignore info context))
                        (log1 :warn "Caught signal ~D during ~
signal-triggered shutdown; don't do this; ignoring the signal"
                              signal)))
                     ;; If TARGET-THREAD is being interrupted by the
                     ;; signal, just abort. Otherwise interrupt
                     ;; TARGET-THREAD with `abort'.
                     (if (eq (bt:current-thread) ,target-thread)
                         (abort)
                         (bt:interrupt-thread
                          ,target-thread #'abort))))))
           ;; Install signal handlers unless on windows.
           #-win32 (mapcar #'install-handler (list ,@signals))
           ,@body)
       ;; Establish an `abort' restart for signal handlers to invoke
       ;; (from TARGET-THREAD or other threads).
       (abort ()))))

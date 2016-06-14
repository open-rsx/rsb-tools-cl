;;;; interactive.lisp --- Functions for interactive stuff.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.tools.common)

(defun call-with-interactive-interrupt-exit (thunk
                                             &key
                                             (signals       `(,sb-posix:SIGHUP
                                                              ,sb-posix:SIGINT
                                                              ,sb-posix:SIGTERM))
                                             (target-thread (bt:current-thread)))
  (restart-case
      (labels
          (#-win32
           (initial-handler (signal info context)
             (declare (ignore info context))
             (log:info "~@<Caught signal ~D; aborting thread ~
                        ~A.~@:>"
                       signal target-thread)
             ;; Install a handler which warns about the repeated
             ;; signal and then ignores it.
             (sb-unix::enable-interrupt signal #'noop-handler)
             ;; If TARGET-THREAD is being interrupted by the signal,
             ;; just abort. Otherwise interrupt TARGET-THREAD with
             ;; `abort'.
             (if (eq (bt:current-thread) target-thread)
                 (abort)
                 (bt:interrupt-thread target-thread #'abort)))
           (noop-handler (signal info context)
             (declare (ignore info context))
             (log:warn "~@<Caught signal ~D during signal-triggered ~
                        shutdown; don't do this; ignoring the ~
                        signal~@:>"
                       signal))
           (install-handler (signal)
             #+win32 (declare (ignore signal))
             #-win32
             (sb-unix::enable-interrupt signal #'initial-handler)))
        ;; Install signal handlers unless on windows.
        #-win32 (mapcar #'install-handler signals)
        (funcall thunk))
    ;; Establish an `abort' restart for signal handlers to invoke
    ;; (from TARGET-THREAD or other threads).
    (abort ())))

(defmacro with-interactive-interrupt-exit
    ((&key
      (signals       nil signals-supplied?)
      (target-thread nil target-thread-supplied?))
     &body body)
  "Run BODY with an interruption handler that exits non-locally and
   returns nil instead of entering the debugger."
  ;; This whole macro is mainly needed for two reasons:
  ;; 1. Protect against multiple signals
  ;; 2. Perform proper shutdown for signals other than SIGINT,
  ;;    especially SIGTERM.
  `(call-with-interactive-interrupt-exit
    (lambda () ,@body)
    ,@(when signals-supplied?       `(:signals ,signals))
    ,@(when target-thread-supplied? `(:target-thread ,target-thread))))

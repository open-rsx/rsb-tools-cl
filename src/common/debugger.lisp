;;;; debugger.lisp --- Disabling the debugger.
;;;;
;;;; Copyright (C) 2011-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.tools.common)

(defun trace-things (specs)
  "Like `trace', but SPECS is evaluated."
  #+sbcl (eval (sb-debug::expand-trace specs))
  #-sbcl (error "Not implemented"))

(defun disable-debugger ()
  "Disable the debugger and return."
  ;; Reenable the debugger which has been disabled when dumping the
  ;; image.
  #+sbcl (sb-ext:enable-debugger)
  ;; Print condition with sane pretty printer state.
  (setf *debugger-hook*
        (lambda (condition previous-value)
          (declare (ignore previous-value))
          (let ((right-margin (max 80 (or *print-right-margin* 0)))
                (miser-width  (min 20 (or *print-miser-width* 0))))
            (with-standard-io-syntax
              (let ((*print-pretty*       t)
                    (*print-right-margin* right-margin)
                    (*print-miser-width*  miser-width))
                (format *error-output* "~&~@<~A~:>~%" condition))))
          (uiop:quit 1))))

;;; Swank

(defun start-swank (&key (port-file "./swank-port.txt"))
  "Start a swank server and write its port to \"./swank-port.txt\"."
  ;; Load swank, if necessary.
  (unless (asdf:component-loaded-p (asdf:find-system :swank))
    (when-let ((quickload (find-symbol (string '#:quickload) '#:ql)))
      (funcall quickload :swank)))
  ;; Delete old port file.
  (when (probe-file port-file)
    (delete-file port-file))
  ;; Start the swank server.
  (uiop:symbol-call '#:swank '#:start-server port-file :dont-close t))

(defun enable-swank-on-signal (&key (signal #+(and sbcl (not win32)) sb-posix:SIGUSR1))
  "Install a handler for SIGNAL that starts a swank server."
  #+(and sbcl (not win32))
  (sb-unix::enable-interrupt
   signal (lambda (signal info context)
            (declare (ignore signal info context))
            (start-swank)))
  #-sbcl
  (warn "~@<Cannot install signal handler to enable SWANK on this ~
         implementation-platfom combination.~@:>"))

;;;; package.lisp --- Package definition for unit tests of the commands module.
;;;;
;;;; Copyright (C) 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb.tools.commands.test
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:more-conditions

   #:lift

   #:rsb.tools.commands)

  (:export
   #:call-with-asynchronously-executing-command
   #:with-asynchronously-executing-command)

  (:export
   #:commands-root)

  (:documentation
   "This package contains unit tests for the commands module."))

(cl:in-package #:rsb.tools.commands.test)

(deftestsuite commands-root ()
  ()
  (:timeout 20)
  (:documentation
   "Root unit test suite for the commands module."))

(defvar *safe-configuration*
  '(((:introspection :enabled)        . "0")
    ((:transport :inprocess :enabled) . "1")
    ((:transport :socket :enabled)    . "0")))

(defvar *introspection-configuration*
  '(((:introspection :enabled)        . "1")
    ((:transport :inprocess :enabled) . "1")
    ((:transport :socket :enabled)    . "0")))

(defun call-with-asynchronously-executing-command (thunk command
                                                   &key
                                                   bindings
                                                   error-policy)
  (let+ ((error)
         ((&flet thread-thunk ()
            (progv (mapcar #'car bindings) (mapcar #'cdr bindings)
              (handler-case
                  (restart-case
                      (handler-bind
                          ((error (lambda (condition)
                                    (when error-policy
                                      (funcall error-policy condition)))))
                        (command-execute command :error-policy error-policy))
                    (abort ()))
                (error (condition)
                  (setf error condition))))))
         (thread (bt:make-thread #'thread-thunk)))
    (prog1
        (unwind-protect (funcall thunk)
          (when (bt:thread-alive-p thread)
            (bt:interrupt-thread thread #'abort))
          (ignore-errors (bt:join-thread thread)))
      (when error (error error)))))

(defmacro with-asynchronously-executing-command
    ((command
      &key
      bindings
      (error-policy nil error-policy-supplied?))
     &body body)
  `(call-with-asynchronously-executing-command
    (lambda () ,@body) ,command
    :bindings (list ,@(mapcar (lambda+ ((var value))
                                `(cons ',var ,value))
                              bindings))
    ,@(when error-policy-supplied?
        `(:error-policy ,error-policy))))

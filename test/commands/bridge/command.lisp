;;;; command.lisp --- Tests for the bridge command class.
;;;;
;;;; Copyright (C) 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.tools.commands.bridge.test)

(deftestsuite commands-bridge-command-root (commands-bridge-root)
  ()
  (:documentation
   "Test suite for the `bridge' command."))

(defun caused-by-forwarding-cycle-error? (condition)
  (typep (cause condition) 'forwarding-cycle-error))

(deftype caused-by-forwarding-cycle-error ()
  `(and specification-error (satisfies caused-by-forwarding-cycle-error?)))

(addtest (commands-bridge-command-root
          :documentation
          "Test construction of the `bridge' command.")
  construction

  (ensure-cases (initargs &optional expected)
      `(;; Some invalid cases.
        (()                                        missing-required-initarg)         ; spec is missing
        ((:spec "")                                specification-error)              ; specification syntax error
        ((:spec "socket: -> socket:")              caused-by-forwarding-cycle-error) ; unidirectional, cycle
        ((:spec "socket: <-> socket:")             caused-by-forwarding-cycle-error) ; bidirectional, cycle
        ((:spec "socket:/foo -> socket:/foo/bar")  caused-by-forwarding-cycle-error) ; unidirectional, maybe cycle
        ((:spec "socket:/foo <-> socket:/foo/bar") caused-by-forwarding-cycle-error) ; bidirectional, maybe cycle

        ;; These are Ok.
        ((:spec "socket:/foo -> socket:/bar")      t)
        ((:spec "socket:/foo <-> socket:/bar")     t)
        ((:spec "socket:/ -> spread:/")            t)
        ((:spec "socket:/ <-> spread:/")           t)
        ((:spec "/foo -> /bar; /baz -> /fez")      t)
        ((:spec "/a -> /b" :max-queued-events 10)  t))

    (let+ (((&flet do-it () (apply #'make-command :bridge initargs))))
      (case expected
        (missing-required-initarg
         (ensure-condition missing-required-initarg (do-it)))
        (specification-error
         (ensure-condition specification-error (do-it)))
        (caused-by-forwarding-cycle-error
         (ensure-condition caused-by-forwarding-cycle-error (do-it)))
        (t
         (ensure (typep (princ-to-string (do-it)) 'string)))))))

(defvar *config*
  '(((:introspection :enabled)        . "0")
    ((:transport :inprocess :enabled) . "1")
    ((:transport :socket :enabled)    . "0")))

(addtest (commands-bridge-command-root
          :documentation
          "Smoke test for the `bridge' command.")
  smoke

  (let* ((rsb:*configuration* *config*)
         (error    nil)
         (command  (make-command :bridge :spec "/foo -> /bar"))
         (thread   (bt:make-thread
                    (lambda ()
                      (let ((rsb:*configuration* *config*))
                        (handler-case
                            (restart-case
                                (command-execute command)
                              (abort ()))
                          (error (condition)
                            (setf error condition)))))))
         (received (lparallel.queue:make-queue)))
    (unwind-protect
         (rsb:with-participants
             ((informer :informer "/foo")
              (listener :listener "/bar"
                        :handlers (list (rcurry #'lparallel.queue:push-queue
                                                received))))
           (sleep 1) ; TODO racy
           (rsb:send informer (rsb:make-event "/foo/baz" 1))
           (loop :until (not (lparallel.queue:queue-empty-p received))))

      (bt:interrupt-thread thread (lambda () (abort)))
      (ignore-errors (bt:join-thread thread)))

    ;; Check for errors.
    (when error (error error))

    ;; Check forwarded event.
    (ensure-same (lparallel.queue:queue-count received) 1)
    (let ((event (lparallel.queue:pop-queue received)))
      (ensure-same (rsb:event-scope event) "/bar/baz" :test #'rsb:scope=)
      (ensure-same (rsb:event-data event) 1)
      (ensure-same (length (rsb:timestamp-alist event)) 5))))

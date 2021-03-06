;;;; command.lisp --- Tests for the bridge command class.
;;;;
;;;; Copyright (C) 2015, 2016 Jan Moringen
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
        (()                                          missing-required-initarg)         ; spec is missing
        ((:spec "")                                  specification-error)              ; specification syntax error
        ((:spec "socket: -> socket:")                caused-by-forwarding-cycle-error) ; unidirectional, cycle
        ((:spec "socket: <-> socket:")               caused-by-forwarding-cycle-error) ; bidirectional, cycle
        ((:spec "socket:/foo -> socket:/foo/bar")    caused-by-forwarding-cycle-error) ; unidirectional, maybe cycle
        ((:spec "socket:/foo <-> socket:/foo/bar")   caused-by-forwarding-cycle-error) ; bidirectional, maybe cycle

        ;; These are Ok.
        ((:spec "socket:/foo-> socket:/bar")         t)
        ((:spec "socket:/foo ->socket:/bar")         t)
        ((:spec "socket:/foo -> socket:/bar")        t)
        ((:spec "socket:/foo ->    socket:/bar")     t)
        ((:spec "socket:/foo <-> socket:/bar")       t)
        ((:spec "socket:/ -> spread:/")              t)
        ((:spec "socket: -> /drop-payload/ spread:") t)
        ((:spec "socket:/ <-> spread:/")             t)
        ((:spec "/foo -> /bar; /baz -> /fez")        t)
        ((:spec "/foo -> /bar ;/baz -> /fez")        t)
        ((:spec "/foo -> /bar ; /baz -> /fez")       t)
        ((:spec "/foo -> /bar ;
                 /baz -> /fez")                      t)
        ((:spec "/a -> /b" :max-queued-events 10)    t))

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

(addtest (commands-bridge-command-root
          :documentation
          "Smoke test for the `bridge' command.")
  smoke

  (let+
      (((&flet test (spec
                     informer-url scope          data
                     listener-url expected-scope expected-data
                     expected-converter-calls)
          (let* ((converter (rsb.converter:make-converter
                             :call-tracking
                             :next (rsb:default-converters)))
                 (rsb:*configuration* *safe-configuration*)
                 (command   (make-command :bridge :spec spec))
                 (received  (lparallel.queue:make-queue)))

            (with-asynchronously-executing-command
                (command :bindings ((rsb:*configuration*       *safe-configuration*)
                                    (rsb::*default-converters* `((t . ,converter)))))
              (rsb:with-participants
                  ((informer :informer informer-url)
                   (listener :listener listener-url
                             :handlers (list (rcurry #'lparallel.queue:push-queue
                                                     received))))
                (sleep 1) ; TODO racy
                (rsb:send informer (rsb:make-event scope data))
                (loop :while (lparallel.queue:queue-empty-p received))))

            ;; Check converter calls.
            (ensure-same (rsb.converter.test:converter-calls converter)
                         (sublis `((:converter . ,converter))
                                 expected-converter-calls)
                         :test #'equal)

            ;; Check forwarded event.
            (ensure-same (lparallel.queue:queue-count received) 1)
            (let ((event (lparallel.queue:pop-queue received)))
              (ensure-same (rsb:event-scope event) expected-scope
                           :test #'rsb:scope=)
              (ensure-same (rsb:event-data event) expected-data)
              (ensure-same (length (rsb:timestamp-alist event)) 5))))))

    (test "socket:/foo -> socket:/bar"
          "socket:/foo" "/foo/baz" 1
          "socket:/bar" "/bar/baz" 1
          ())
    (test "socket:/foo -> /drop-payload/ socket:/bar"
          "socket:/foo" "/foo/baz" 1
          "socket:/bar" "/bar/baz" rsb.converter:+no-value+
          `((rsb.converter:domain->wire
             :converter ,rsb.transform:+dropped-payload+)))))

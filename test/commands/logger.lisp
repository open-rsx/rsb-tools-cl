;;;; logger.lisp --- Tests for the logger command class.
;;;;
;;;; Copyright (C) 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.tools.commands.test)

(deftestsuite logger-root (commands-root)
  ()
  (:documentation
   "Test suite for the `logger' command."))

(addtest (logger-root
          :documentation
          "Test construction of the `logger' command.")
  construction

  (ensure-cases (initargs &optional expected)
      `(;; Some invalid cases.
        (()                              missing-required-initarg) ; :uris is missing
        ((:uris       (,(puri:uri "/"))) missing-required-initarg) ; :style or :style-spec is missing
        ((:style-spec "detailed")        missing-required-initarg) ; :uris is missing
        ;; These are Ok.
        ((:uris              (,(puri:uri "/"))
          :style-spec        "detailed"))
        ((:uris              (,(rsb:make-scope "/"))
          :style-spec        "detailed"))
        ((:uris              ("/")
          :style-spec        "detailed"))
        ((:uris              (,(puri:uri "/"))
          :style             ,(rsb.formatting:make-style :detailed)))
        ((:uris              (,(puri:uri "/"))
          :style-spec        "detailed"
          :max-queued-events 100))
        ((:uris              (,(puri:uri "/"))
          :style-spec        "detailed"
          :filters           (,(rsb.filter:filter :scope :scope "/"))))
        ((:uris              (,(puri:uri "/"))
          :style-spec        "detailed"
          :stream            ,*error-output*))
        ((:uris              (,(puri:uri "/"))
          :style-spec        "detailed"
          :stream-spec       :error-output))
        ((:uris              (,(puri:uri "/"))
          :style-spec        "detailed"
          :while             ,(lambda (count event)
                                (declare (ignore count event))))))

    (let+ (((&flet do-it () (apply #'make-command :logger initargs))))
      (case expected
        (missing-required-initarg
         (ensure-condition missing-required-initarg (do-it)))
        (t
         (ensure (typep (princ-to-string (do-it)) 'string)))))))

(addtest (logger-root
          :documentation
          "Smoke test of the `logger' command class.")
  smoke

  (let* ((scope               "/rsbtest/tools/commands/logger")
         (configuration       *safe-configuration*)
         (rsb:*configuration* configuration))
    (ensure-cases (initargs events expected
                   &key
                   (expected-event-count (length events)))
        `(;; Terminate after receiving and logging a single event on
          ;; expected scope.
          ((:uris       (,(puri:uri scope))
            :style-spec (:programmable/template
                         :template "${sequence-number} ${data}\\n"))
           (,(rsb:make-event scope 1))
           ,(format nil "0 1~%"))

          ;; Terminate after receiving but filtering one event and
          ;; receiving and logging a second.
          ((:uris       (,(puri:uri scope))
            :filters    (,(rsb.filter:filter :method :method nil))
            :style-spec (:programmable/template
                         :template "${sequence-number} ${data}\\n"))
           (,(rsb:make-event scope 1)
            ,(rsb:make-event scope 2 :method :unexpected))
           ,(format nil "0 1~%")
           :expected-event-count 1)

          ;; Continue after an error.
          ((:uris       (,(puri:uri scope))
            :style-spec (:programmable/template
                         :template ,(format nil "${(when (eql data 1) ~
                                                     (error \"intentional error\"))} ~
                                                 ${sequence-number} ${data}\\n")))
           (,(rsb:make-event scope 1)
            ,(rsb:make-event scope 2))
           ,(format nil "NIL 1 2~%")))

      (let* ((output  (make-string-output-stream))
             (while   (lambda (i event)
                        (declare (ignore event))
                        (< i expected-event-count)))
             (command (apply #'make-command :logger
                             (append initargs `
                                     (:stream ,output :while ,while))))
             (sender  (bt:make-thread
                       (lambda ()
                         (sleep 1)      ; TODO racy
                         (let ((rsb:*configuration* configuration))
                           (rsb:with-participant (i :informer scope)
                             (mapc (curry #'rsb:send i) events)))))))
        (unwind-protect
             (progn
               (handler-bind ((error #'continue))
                 (command-execute command :error-policy #'continue))
               (ensure-same (get-output-stream-string output) expected
                            :test #'string=))
          (bt:join-thread sender))))))

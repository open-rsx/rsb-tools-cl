;;;; introspect.lisp --- Tests for the introspect command class.
;;;;
;;;; Copyright (C) 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.tools.commands.test)

(deftestsuite introspect-root (commands-root)
  ()
  (:documentation
   "Test suite for the `introspect' command."))

(addtest (introspect-root
          :documentation
          "Test construction of the `introspect' command.")
  construction

  (ensure-cases (initargs &optional expected)
      `(;; Some invalid cases.
        (()                              missing-required-initarg) ; :uris,:style[-spec] are missing
        ((:uris       (,(puri:uri "/"))) missing-required-initarg) ; :style[-spec] is missing

        ;; These are Ok.
        ((:uris        (,(puri:uri "/"))
          :style-spec  "object-tree"))
        ((:uris        (,(puri:uri "/"))
          :style       ,(rsb.formatting:make-style
                         :object-tree
                         :service 'rsb.formatting.introspection::style)))
        ((:uris        (,(puri:uri "/"))
          :style-spec  "object-tree"
          :stream      ,*standard-output*))
        ((:uris        (,(puri:uri "/"))
          :style-spec  "object-tree"
          :stream-spec :error-output))
        ((:uris        (,(puri:uri "/"))
          :style-spec  "object-tree :stateful? nil")))

    (let+ (((&flet do-it () (apply #'make-command :introspect initargs))))
      (case expected
        (missing-required-initarg
         (ensure-condition missing-required-initarg (do-it)))
        (t
         (ensure (typep (princ-to-string (do-it)) 'string)))))))

(addtest (introspect-root
          :documentation
          "Smoke test for the `introspect' command.")
  smoke

  (let* ((configuration *introspection-configuration*)
         (stream        (make-string-output-stream))
         (command       (make-command :introspect
                                      :uris       '("/")
                                      :style-spec "monitor/events"
                                      :stream     stream)))
    (with-asynchronously-executing-command
        (command :bindings ((rsb:*configuration* configuration)))
      (sleep 1) ; TODO racy
      (let ((rsb:*configuration* configuration))
        (rsb:with-participant
            (nil :listener "/rsbtest/tools/commands/introspect/listener"))))
    (ensure (not (emptyp (get-output-stream-string stream))))))

;;;; command.lisp --- Tests for the web command class.
;;;;
;;;; Copyright (C) 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.tools.commands.web.test)

(deftestsuite commands-web-command-root (commands-web-root)
  ()
  (:documentation
   "Test suite for the `web' command."))

(addtest (commands-web-command-root
          :documentation
          "Test construction of the `web' command.")
  construction

  (ensure-cases (initargs &optional expected)
      `(;; Some invalid cases.
        (() missing-required-initarg) ; :uris is missing

        ;; These are Ok.
        ((:uris             (,(puri:uri "/"))))
        ((:uris             (,(puri:uri "/"))
          :address          nil))
        ((:uris             (,(puri:uri "/"))
          :address          "0.0.0.0"))
        ((:uris             (,(puri:uri "/"))
          :port             4567))
        ((:uris             (,(puri:uri "/"))
          :document-root    #"static/"))
        ((:uris             (,(puri:uri "/"))
          :document-root    nil))
        ((:uris             (,(puri:uri "/"))
          :response-timeout 1.0)))

    (let+ (((&flet do-it () (apply #'make-command :web initargs))))
      (case expected
        (missing-required-initarg
         (ensure-condition missing-required-initarg (do-it)))
        (t
         (ensure (typep (princ-to-string (do-it)) 'string)))))))

(addtest (commands-web-command-root
          :documentation
          "Smoke test for the `web' command.")
  smoke

  (let* ((port          (usocket:with-socket-listener (socket "localhost" 0)
                          (usocket:get-local-port socket))) ; TODO racy
         (configuration *introspection-configuration*)
         (command       (make-command :web :uris '("/") :port port)))
    (with-asynchronously-executing-command
        (command :bindings ((rsb:*configuration* configuration)))
      (let+ (((&flet request (path accept
                              &key
                              (method          :get)
                              query-parameters)
                (let+ ((uri (puri:copy-uri (puri:uri "http://localhost")
                                           :port  port
                                           :path  path
                                           :query query-parameters))
                       ((&values body code headers)
                        (drakma:http-request uri :accept accept)))
                  (ensure (< 199 code 300))
                  body)))
             ((&flet request/json (path &rest args &key)
                (json:decode-json-from-source
                 (flexi-streams:make-flexi-stream
                  (flexi-streams:make-in-memory-input-stream
                   (apply #'request path "application/json" args)))))))
        (sleep 1) ; TODO racy
        (let ((rsb:*configuration* configuration))
          (rsb:with-participant
              (nil :listener "/rsbtest/tools/commands/web/listener")))
        (sleep 1) ; TODO racy

        ;; Test endpoint /api/introspection/snapshot
        (let ((endpoint "/api/introspection/snapshot"))
          (request/json endpoint))))))

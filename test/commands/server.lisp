;;;; server.lisp --- Tests for the server command class.
;;;;
;;;; Copyright (C) 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.tools.commands.test)

(deftestsuite server-root (commands-root)
  ()
  (:documentation
   "Test suite for the `server' command."))

(addtest (server-root
          :documentation
          "Test construction of the `server' command.")
  construction

  (ensure-cases (initargs &optional expected)
      `(;; Some invalid cases.
        (()     missing-required-initarg)          ; :uris is missing
        ((:uris (,(puri:uri "socket:?server=0")))) ; server mode
        ((:uris ("socket:?server=0")))             ; likewise
        ;; These are Ok.
        ((:uris (,(puri:uri "/"))))
        ((:uris (,(rsb:make-scope "/"))))
        ((:uris ("/"))))

    (let+ (((&flet do-it () (apply #'make-command :server initargs))))
      (case expected
        (missing-required-initarg
         (ensure-condition missing-required-initarg (do-it)))
        (t
         (ensure (typep (princ-to-string (do-it)) 'string)))))))

(defvar *port-promise*)

(defun note-port (port)
  (lparallel:fulfill *port-promise* port))

(defvar *port-and-portfile-configuration*
  '(((:transport :socket :enabled)  . t)
    ((:transport :socket :port)     . 0)
    ((:transport :socket :portfile) . "call:rsb.tools.commands.test::note-port")))

(addtest (server-root
          :documentation
          "Smoke test for the `server' command.")
  smoke

  (let+ (((&flet obtain-url (promise)
            (when-let ((port (lparallel:force promise)))
              (format nil "socket://localhost:~D/?server=0" port))))
         ((&flet+ test ((uris configuration &optional expected))
            (let* ((command (make-command :server :uris uris))
                   (error   nil)
                   (promise (lparallel:promise))
                   (thread  (bt:make-thread
                             (lambda ()
                               (restart-case
                                   (handler-bind
                                       ((warning (lambda (condition)
                                                   (muffle-warning)))
                                        (error   (lambda (condition)
                                                   (setf error condition)
                                                   (lparallel:fulfill promise)
                                                   (abort))))
                                     (let ((rsb:*configuration* configuration)
                                           (*port-promise*      promise))
                                       (command-execute command)))
                                 (abort ()))))))
              ;; Make sure communication works.
              (unwind-protect
                   (let ((rsb:*configuration* *safe-configuration*)
                         (url                 (obtain-url promise)))
                     (when url
                       (rsb:with-participants ((informer :informer url)
                                               (reader   :reader   url))
                         (rsb:send informer 1)
                         (ensure-same (rsb:event-data (rsb:receive reader)) 1))))

                (ignore-errors (bt:interrupt-thread thread #'abort))
                (ignore-errors (bt:join-thread thread)))

              ;; Check for errors.
              (case expected
                (error
                 (ensure-condition error (when error (error error))))
                (t
                 (when error (error error))))))))

    (mapc
     #'test
     `(;; Scopes
       (("/")
        ,*port-and-portfile-configuration*)
       ((,(rsb:make-scope "/"))
        ,*port-and-portfile-configuration*)

       (("/ignored")
        ,*port-and-portfile-configuration*)
       ((,(rsb:make-scope "/ignored"))
        ,*port-and-portfile-configuration*)

       (("/")
        ,*safe-configuration*
        error) ; inprocess instead of socket transport
       ((,(rsb:make-scope "/"))
        ,*safe-configuration*
        error)

       ;; URIs
       (("socket://localhost:0?portfile=call:rsb.tools.commands.test::note-port")
        ,*safe-configuration*)

       (("socket://localhost:0/ignored?portfile=call:rsb.tools.commands.test::note-port")
        ,*safe-configuration*)

       (("inprocess:")
        ,*safe-configuration*
        error) ; wrong transport

       (("socket://localhost:0/?server=0")
        ,*safe-configuration*
        error) ; server=0

       (("socket://localhost:0/?server=auto")
        ,*safe-configuration*
        error))))) ; server=auto

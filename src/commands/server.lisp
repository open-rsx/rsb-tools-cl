;;;; server.lisp --- Implementation of the server command.
;;;;
;;;; Copyright (C) 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.tools.commands)

;;; `server' command class

(defclass server (source-mixin
                  print-items:print-items-mixin)
  ()
  (:documentation
   "Accept and service socket transport clients on one or more sockets."))

(service-provider:register-provider/class
 'command :server :class 'server)

(defmethod command-execute ((command server) &key error-policy)
  (let+ (((&accessors-r/o (uris command-uris)) command)
         (uris         (mapcar #'%ensure-server-mode uris))
         (participants '()))
    (unwind-protect
         (progn
           (mapc (lambda (uri)
                   (format t "Creating server for ~A~%" uri)
                   (push (make-participant
                          :informer uri
                          :introspection? nil
                          :error-policy   error-policy)
                         participants))
                 uris)
           (format t "Ready~%")
           (loop (sleep 1)))
      (mapc #'detach/ignore-errors participants))))

;;; Utilities

(defun %ensure-server-mode (scope-or-uri)
  (let+ (((&flet check-scope (scope)
            (unless (scope= scope "/")
              (warn "~@<Ignoring scope ~A in ~A.~@:>"
                    (scope-string scope) scope-or-uri))))
         ((&flet+ check-transport ((transport &rest options))
            (unless (member transport '(:socket :tcp-socket
                                        :unix :unix-socket))
              (error "~@<Server mode is not supported for transport ~
                      ~A requested via ~A.~@:> "
                     transport scope-or-uri))
            (when (member (getf options :server :missing)
                          '(nil "0" "false" "auto")
                          :test #'equal)
              (error "~@<Cannot create transport according to ~A: ~
                      server mode must be enabled.~:@>"
                     scope-or-uri))))
         ((&flet check-options (raw-options)
            (let* ((all-options (rsb::effective-transport-options
                                 (rsb::merge-transport-options
                                  raw-options (transport-options)))))
              (mapc #'check-transport all-options)))))
    (etypecase scope-or-uri
      (puri:uri
       (let+ (((&values scope options)
               (rsb:uri->scope-and-options scope-or-uri)))
         (check-scope scope)
         (check-options options)
         (let ((query (format nil "~A~@[&~A~]"
                              "server=1" (puri:uri-query scope-or-uri))))
           (puri:copy-uri scope-or-uri :path "/" :query query))))
      (scope
       (check-scope scope-or-uri)
       (check-options '())
       (make-instance 'puri:uri :path "/" :query "server=1")))))

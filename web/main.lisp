;;;; main.lisp --- Entry point of the web tool.
;;;;
;;;; Copyright (C) 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.tools.web)

(defun make-help-string (&key (show :default))
  (with-output-to-string (stream)
    (format stream "Serve system information such as introspection via HTTP. ~
                    Collect information via the transports designated ~
                    by URI* (zero or more URIs).~@
                    ~@
                    When no URIs are supplied, the default transport ~
                    configuration is used.~@
                    ~@
                    ")
    (with-abbreviation (stream :uri show)
      (format stream "URI* are of the form~@
                      ~@
                      ")
      (print-uri-help stream :uri-var "URI*"))))

(defun make-examples-string (&key (program-name "rsb web"))
  (format nil
          "~2@T~A~@
           ~@
           Use the default transport configuration to gather a ~
           information about the system and serve it on the default ~
           address and port (i.e. http://localhost:4444).~@
           ~@
           ~2@T~:*~A socket: spread://somehost~@
           ~@
           Gather information via two transports: the socket transport ~
           and the Spread transport. The gathered information is ~
           merged as if all collected processes and participants were ~
           participating in a single RSB bus.~@
           "
          program-name))

(defun update-synopsis (&key
                        (show         :default)
                        (program-name "rsb web"))
  "Create and return a commandline option tree."
  (make-synopsis
   ;; Basic usage and specific options.
   :postfix "URI*"
   :item    (make-text :contents (make-help-string :show show))
   :item    (make-common-options :show show)
   :item    (make-error-handling-options :show show)
   :item    (defgroup (:header "Introspection Options")
              (lispobj :long-name       "response-timeout"
                       :typespec        'positive-real
                       :default-value   .5
                       :argument-name   "SECONDS"
                       :description
                       "Time in seconds to wait for responses to introspection requests.

In most systems, all replies should arrive within a few milliseconds. However, circumstances like heavily loaded system, degraded system performance or extreme communication latency may require larger values."))
   :item    (defgroup (:header "HTTP Server Options")
              (stropt  :long-name       "address"
                       :default-value   "localhost"
                       :argument-name   "ADDRESS"
                       :description
                       "Address on which the HTTP server should listen.")
              (lispobj :long-name       "port"
                       :typespec        '(integer 0 65535)
                       :default-value   4444
                       :argument-name   "PORT"
                       :description
                       "Port on which the HTTP server should listen.")
              (path    :long-name       "document-root"
                       :type            :directory
                       :argument-name   "DIRECTORY"
                       :description
                       "Directory from which static content such as HTML pages and CSS files should be read.")
              (path    :long-name       "message-log-file"
                       :type            :file
                       :description
                       "Name of a file to which web server message should be logged.")
              (path    :long-name       "access-log-file"
                       :type            :file
                       :description
                       "Name of a file to which web server accesses should be logged."))
   ;; Append RSB options.
   :item    (make-options :show? (show-help-for? :rsb :show show))
   ;; HTTP endpoints.
   :item    (defgroup (:header "HTTP Endpoints")
              (make-text :contents"http://ADDRESS:PORT/static

  Contents of the directory specified via static-directory is made available here.

http://ADDRESS:PORT/introspection/json

  A JSON-serialization of a snapshot of the introspection data for the system or systems specified via URI can be obtained here."))
   ;; Append examples.
   :item    (defgroup (:header "Examples")
              (make-text :contents (make-examples-string
                                    :program-name program-name)))))

(defun main (program-pathname args)
  "Entry point function of the cl-rsb-tools-web system."
  (let ((program-name (concatenate
                       'string (namestring program-pathname) " web")))
    (update-synopsis :program-name program-name)
    (setf *configuration* (options-from-default-sources))
    (process-commandline-options
     :commandline     (list* program-name args)
     :version         (cl-rsb-tools-web-system:version/list :commit? t)
     :update-synopsis (curry #'update-synopsis :program-name program-name)
     :return          (lambda () (return-from main)))
    (enable-swank-on-signal))

  (let ((error-policy     (maybe-relay-to-thread
                           (process-error-handling-options)))
        (uris             (or (remainder) (list "/")))
        (response-timeout (getopt :long-name "response-timeout"))
        (address          (getopt :long-name "address"))
        (port             (getopt :long-name "port"))
        (document-root    (getopt :long-name "document-root"))
        (message-log-file (getopt :long-name "message-log-file"))
        (access-log-file  (getopt :long-name "access-log-file")))
    (rsb.formatting:with-print-limits (*standard-output*)
      (with-logged-warnings
        (with-error-policy (error-policy)
          (let ((command (make-command :web
                                       :uris             uris
                                       :response-timeout response-timeout
                                       :address          address
                                       :port             port
                                       :document-root    document-root
                                       :message-log      message-log-file
                                       :access-log       access-log-file)))
            (with-interactive-interrupt-exit ()
              (command-execute command :error-policy error-policy))))))))

;;;; main.lisp --- Entry point of the web tool.
;;;;
;;;; Copyright (C) 2015, 2016, 2019 Jan Moringen
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
                       "Directory from which static content such as HTML pages and CSS files should be read.

If this option is not supplied, a built-in version of the static content is usually used.")
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
              (make-text :contents"http://ADDRESS:PORT/

  Either the contents of the directory specified via the document-root option or built-in resource files are made available here.

http://ADDRESS:PORT/api/introspection/snapshot

  A JSON-serialization of a snapshot of the introspection data for the system or systems specified via URI can be obtained here.

http://ADDRESS:PORT/api/introspection/search

  Query the introspection database using XPath, receive JSON-serialized results.

  Return an atomic result for expressions not evaluating to node sets and an array of matches otherwise. An atomic result can be a number or string. For example, the result of the query

    count(//@foo)

  is a number. A match can be an attribute match or an element match.

  Accepted query parameters:

  * query [required]

    The (non-empty) query string.

    One of the following things:

    * An XPath expression.

    * One or more words: match any node (element, attribute, text) containing all words.

  * start: non-negative-integer [optional]

    Index of first node in match sequence that should be returned.

  * limit: positive-integer [optional]

    Number of nodes from the match sequence that should be returned.

For all /api/** endpoints at least the content types text/html and application/json are supported.

If the Accept header indicates that the response content type should be HTML, the response body is a HTML document containing a human-readable description of the endpoint.

If the Accept header indicates that the response content type should be JSON, the response body is of one of the forms

  {\"data\": DATA}
  {\"error\": DESCRIPTION}
  {\"data\": DATA, \"error\": DESCRIPTION}

i.e. at least one of the \"data\" and \"error\" properties is present. Both can be present if an error occurs while streaming the body of an initially successful response."))
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
     :version         (rsb-tools-web-system:version/list :commit? t)
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

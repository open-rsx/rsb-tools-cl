;;;; main.lisp --- Entry point of the introspect tool.
;;;;
;;;; Copyright (C) 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.tools.introspect)

(defun make-help-string (&key (show :default))
  (with-output-to-string (stream)
    (format stream "Display introspection information for hosts, ~
                    processes and RSB participants reachable via the ~
                    transports designated by URI* (zero or more URIs). ~@
                    ~@
                    When no URIs are supplied, the default transport ~
                    configuration is used.
                    ~@
                    ")
    (with-abbreviation (stream :uri show)
      (format stream "URI* are of the form~@
                      ~@
                      ")
      (print-uri-help stream :uri-var "URI*"))))

(defun make-style-help-string (&key (show :default))
  (with-output-to-string (stream)
    (rsb.common:with-abbreviation (stream :styles show)
      (format stream "The following formatting styles are currently ~
                      available:~@
                      ~@
                      ")
      (let* ((providers (service-provider:service-providers
                         'rsb.formatting.introspection::style))
             (classes   (mapcar (lambda (provider)
                                  (list
                                   (service-provider:provider-name provider)
                                   (service-provider:provider-class provider)))
                                providers)))
        (rsb.common:print-classes-help-string
         classes stream :initarg-blacklist '(:database))))))

(defun make-examples-string (&key (program-name "rsb introspect"))
  (format nil
          "~2@T~A~@
           ~@
           Use the default transport configuration to gather a ~
           snapshot of the system state and print a tree of hosts, ~
           processes and participants.~@
           ~@
           ~2@T~:*~A socket: spread://somehost~@
           ~@
           Gather introspection information via two transports: the ~
           socket transport and the Spread transport. The gathered ~
           information is merged as if all collected processes and ~
           participants were participating in a single RSB bus.~@
           ~@
           ~2@T~:*~A --style monitor/object-tree~@
           ~@
           Like the first example, but instead of printing one ~
           snapshot and exiting, continue gathering introspection ~
           information and periodically print an updated object ~
           tree.~@
           ~@
           ~2@T~:*~A --style monitor/events~@
           ~@
           Continuously collect introspection information and print ~
           information about significant changes in the observed ~
           system. Significant changes include start and termination ~
           of processes and addition and removal of participants.~@
           "
          program-name))

(defun update-synopsis (&key
                        (show         :default)
                        (program-name "rsb introspect"))
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

In most systems, all replies should arrive within a few milliseconds. However, circumstances like heavily loaded system, degraded system performance or extreme communication latency may require larger values.")
              (stropt  :long-name       "style"
                       :short-name      "s"
                       :default-value   "object-tree"
                       :argument-name   "SPEC"
                       :description
                       (make-style-help-string :show show)))
   ;; Append RSB options.
   :item    (make-options :show? (show-help-for? :rsb :show show))
   ;; Append examples.
   :item    (defgroup (:header "Examples")
              (make-text :contents (make-examples-string
                                    :program-name program-name)))))

(defun main (program-pathname args)
  "Entry point function of the cl-rsb-tools-introspect system."
  (let ((program-name (concatenate
                       'string (namestring program-pathname) " introspect")))
    (update-synopsis :program-name program-name)
    (setf *configuration* (options-from-default-sources))
    (process-commandline-options
     :commandline     (list* program-name args)
     :version         (cl-rsb-tools-introspect-system:version/list :commit? t)
     :update-synopsis (curry #'update-synopsis :program-name program-name)
     :return          (lambda () (return-from main)))
    (enable-swank-on-signal))

  (let* ((error-policy     (maybe-relay-to-thread
                            (process-error-handling-options)))
         (uris             (or (remainder) (list "/")))
         (style            (getopt :long-name "style"))
         (response-timeout (getopt :long-name "response-timeout"))
         (command          (make-command :introspect
                                         :uris             uris
                                         :style-spec       style
                                         :response-timeout response-timeout)))
    (rsb.formatting:with-print-limits (*standard-output*)
      (with-logged-warnings
        (with-error-policy (error-policy)
          (with-interactive-interrupt-exit ()
            (command-execute command :error-policy error-policy)))))))

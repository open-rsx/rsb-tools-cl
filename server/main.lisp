;;;; main.lisp --- Entry point of the server tool.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.tools.server)

(defun make-help-string (&key
                         (show :default))
  "Return a help that explains the commandline option interface."
  (with-output-to-string (stream)
    (format stream "Run a server which accepts clients according to ~
                    URIs.~@
                    ~@
                    Currently only supports the RSB socket transport.~@
                    ~@
                    ")
    (with-abbreviation (stream :uri show)
      (progn
        (format stream "A URI is of the form~@
                        ~@
                        ~2@T")
        (print-uri-help stream :uri-var "URI")))))

(defun make-examples-string (&key (program-name "rsb server"))
  "Make and return a string containing usage examples of the program."
  (format nil "~2@T~A~@
               ~@
               Unless the RSB configuration is non-default, accept ~
               socket connections on the default port.~@
               ~@
               ~2@T~:*~A 'socket://localhost:0?portfile=/tmp/port.txt'~@
               ~@
               Accept socket connections on an available port which is ~
               written into the file /tmp/port.txt after it has been ~
               chosen.~@
               ~@
               Note the use of single quotes (') to prevent the shell ~
               from interpreting syntactically relevant characters in ~
               the URI."
          program-name))

(defun update-synopsis (&key
                        (show         :default)
                        (program-name "rsb server"))
  "Create and return a commandline option tree."
  (make-synopsis
   ;; Basic usage and specific options.
   :postfix "URI*"
   :item    (make-text :contents (make-help-string :show show))
   :item    (make-common-options :show show)
   :item    (make-error-handling-options :show show)
   ;; Append RSB options.
   :item    (make-options
             :show? (or (eq show t)
                        (and (listp show) (member :rsb show))))
   ;; Append examples.
   :item    (defgroup (:header "Examples")
              (make-text :contents (make-examples-string
                                    :program-name program-name)))))

(defun main (program-pathname args)
  "Entry point function of the cl-rsb-tools-server system."
  (let ((program-name (concatenate
                       'string (namestring program-pathname) " server")))
    (update-synopsis :program-name program-name)
    (setf *configuration* (options-from-default-sources))
    (process-commandline-options
     :commandline     (list* program-name args)
     :version         (cl-rsb-tools-server-system:version/list :commit? t)
     :update-synopsis (curry #'update-synopsis :program-name program-name)
     :return          (lambda () (return-from main)))
    (enable-swank-on-signal))

  (let+ ((error-policy (maybe-relay-to-thread
                        (process-error-handling-options)))
         (uris         (or (remainder) (list "/"))))
    (rsb.formatting:with-print-limits (*standard-output*)
      (with-logged-warnings
        (with-error-policy (error-policy)
          (let ((command (make-command :server :uris uris)))
            (with-interactive-interrupt-exit ()
              (command-execute command :error-policy error-policy))))))))

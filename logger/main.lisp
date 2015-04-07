;;;; main.lisp --- Entry point of the logger tool.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.tools.logger)

(defun update-synopsis (&key
                        (show :default))
  "Create and return a commandline option tree."
  (make-synopsis
   ;; Basic usage and specific options.
   :postfix "URI*"
   :item    (make-text :contents (make-help-string :show show))
   :item    (make-common-options :show show)
   :item    (make-error-handling-options :show show)
   :item    (defgroup (:header "Logging Options"
                       :hidden (not (show-help-for?
                                     '(:logging :filters :styles :columns :quantities)
                                     :default t
                                     :show    show)))
              (stropt  :long-name       "filter"
                       :short-name      "f"
                       :argument-name   "SPEC"
                       :description
                       (make-filter-help-string :show show))
              (stropt  :long-name       "style"
                       :short-name      "s"
                       :default-value   "compact"
                       :argument-name   "SPEC"
                       :description
                       (make-style-help-string :show show))
              (lispobj :long-name      "max-queued-events"
                       :typespec       '(or null positive-integer)
                       :default-value  200
                       :argument-name  "NUMBER-OF-EVENTS"
                       :description
                       "The maximum number of events which may be queued for processing at any given time. Note that choosing a large value can require a large amount of memory."))
   :item    (make-idl-options)
   ;; Append RSB options.
   :item    (make-options
             :show? (show-help-for? :rsb :show show))
   ;; Append examples.
   :item    (defgroup (:header "Examples")
              (make-text :contents (make-examples-string)))))

(defun main (program-pathname args)
  "Entry point function of the cl-rsb-tools-logger system."
  (update-synopsis)
  (setf *configuration* (options-from-default-sources))
  (process-commandline-options
   :commandline     (list* (concatenate
                            'string (namestring program-pathname) " logger")
                           args)
   :version         (cl-rsb-tools-logger-system:version/list :commit? t)
   :update-synopsis #'update-synopsis
   :return          (lambda () (return-from main)))
  (enable-swank-on-signal)

  (let* ((error-policy      (maybe-relay-to-thread
                             (process-error-handling-options)))
         (uris              (or (remainder) (list "/")))
         (filters           (iter (for spec next (getopt :long-name "filter"))
                                  (while spec)
                                  (collect (apply #'rsb.filter:filter
                                                  (parse-instantiation-spec spec)))))
         (event-style       (getopt :long-name "style"))
         (max-queued-events (getopt :long-name "max-queued-events"))
         (command           (apply #'make-command :logger
                                   :uris              uris
                                   :style-spec        event-style
                                   :max-queued-events max-queued-events
                                   (when filters
                                     (list :filters filters)))))
    (with-print-limits (*standard-output*)
      (with-logged-warnings
        (with-error-policy (error-policy)

          ;; Load IDLs as specified on the commandline. TODO should the command do this?
          (process-idl-options)

          ;; The commands creates the required participants and
          ;; starts the receiving and printing loop.
          (with-interactive-interrupt-exit ()
            (command-execute command :error-policy error-policy)))))))

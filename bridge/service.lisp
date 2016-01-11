;;;; service.lisp --- Entry point of the bridge service command.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.tools.bridge)

(defun update-synopsis/service (&key
                                (show         :default)
                                (program-name "rsb bridge-service"))
  "Create and return a commandline option tree."
  (make-synopsis
   ;; Basic usage and specific options.
   :postfix "CONTROL-URI"
   :item    (make-text :contents (make-help-string :show show))
   :item    (make-common-options :show show)
   :item    (make-error-handling-options :show show)
   :item    (defgroup (:header "Bridge Options"
                               :hidden (not (show-help-for? '(:bridge)
                                                            :default t
                                                            :show    show)))
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
   :item    (defgroup (:header "Remote Interface")
              (make-text :contents (format nil "~
The remote interface consists of two parts: 1) an event-based ~
interface for change notification 2) a request-reply interface for ~
manipulating the bridge and its connections.~@_
~@_
Event-based Interface~@_
~@_
  State changes are communicated using an event-based interface below ~
  the scope CONTROL-SCOPE/state with the following sub-scopes:~@_
  ~@_
  CONTROL-SCOPE/state/ready : void~@_
    ~@_
    An event without payload is published on this scope when the ~
    bridge and its remote interface become available.~@_
  ~@_
  CONTROL-SCOPE/state/terminated : void~@_
    ~@_
    An event without payload is published on this scope when bridge ~
    terminates.~@_
  ~@_
  CONTROL-SCOPE/state/child-added : scope~@_
    ~@_
    When a connection is added to the bridge, an event containing the ~
    control scope for the new connection as its payload is published ~
    on this scope.~@_
  ~@_
  CONTROL-SCOPE/state/child-removed : scope~@_
    ~@_
    When a connection is removed from the bridge, an event container ~
    the control scope for the removed connection as its payload is ~
    published on this scope.~@_
~@_
Request/Reply Interface~@_
  ~@_
  A local server on the scope CONTROL-SCOPE allows remote manipulation ~
  of the bridge and its connections. The server provides the following ~
  methods:~@_
  ~@_
  terminate : void -> void~@_
    ~@_
    Terminate the bridge process.~@_
  ~@_
  connect : string -> scope~@_
    ~@_
    Create a forwarding connection according to the specification ~
    provided in the argument. Return a scope at which a remote ~
    interface for managing the connection is provided.~@_
  ~@_
  destroy : scope -> void [TEMPORARY, SUBJECT TO CHANGE]~@_
    ~@_
    Destroy the forwarding connection designated by the argument.")))
   ;; Append examples.
   :item    (defgroup (:header "Examples")
              (make-text :contents (make-examples-string/service
                                    :program-name program-name)))))

(defun main/service (program-pathname args)
  "Service entry point function of the cl-rsb-tools-bridge system."
  (let ((program-name (concatenate
                       'string (namestring program-pathname) " bridge-service")))
    (update-synopsis/service :program-name program-name)
    (setf *configuration* (options-from-default-sources))
    (process-commandline-options
     :commandline     (list* program-name args)
     :version         (cl-rsb-tools-bridge-system:version/list :commit? t)
     :update-synopsis (curry #'update-synopsis/service :program-name program-name)
     :return          (lambda () (return-from main/service)))
    (enable-swank-on-signal))

  (let* ((error-policy      (maybe-relay-to-thread
                             (process-error-handling-options)))
         (control-uri       (first (remainder)))
         (max-queued-events (getopt :long-name "max-queued-events")))
    (rsb.formatting:with-print-limits (*standard-output*)
      ;; Check control URI option.
      (unless control-uri
        (error "~@<Supply a control URI as remainder of the ~
                commandline.~@:>"))

      (with-logged-warnings
        (with-error-policy (error-policy)
          ;; Load IDLs as specified on the commandline.
          (process-idl-options)

          (let ((command (make-command :bridge-service
                                       :destination       control-uri
                                       :max-queued-events max-queued-events)))
            (with-interactive-interrupt-exit ()
              (command-execute command :error-policy error-policy))))))))

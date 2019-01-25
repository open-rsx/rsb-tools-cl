;;;; main.lisp --- Entry point of the bridge tool.
;;;;
;;;; Copyright (C) 2011-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.tools.bridge)

(defun update-synopsis (&key
                        (show         :default)
                        (program-name "rsb bridge"))
  "Create and return a commandline option tree."
  (make-synopsis
   ;; Basic usage and specific options.
   :postfix "[SIMPLE-FORWARDING-SPECIFICATION]"
   :item    (make-text :contents (make-help-string :show show))
   :item    (make-common-options :show show)
   :item    (make-error-handling-options :show show)
   :item    (defgroup (:header "Bridge Options"
                       :hidden (not (show-help-for? '(:bridge :filters)
                                                    :default t
                                                    :show    show)))
              (lispobj :long-name      "max-queued-events"
                       :typespec       '(or null positive-integer)
                       :default-value  2000
                       :argument-name  "NUMBER-OF-EVENTS"
                       :description
                       "The maximum number of events which may be queued for processing at any given time. Note that choosing a large value can require a large amount of memory."))
   :item    (make-idl-options)
   ;; Append RSB options.
   :item    (make-options
             :show? (show-help-for? :rsb :show show))
   ;; Append examples.
   :item    (defgroup (:header "Examples")
              (make-text :contents (make-examples-string
                                    :program-name program-name)))))

(defun main (program-pathname args)
  "Entry point function of the cl-rsb-tools-bridge system."
  (let ((program-name (concatenate
                       'string (namestring program-pathname) " bridge")))
    (update-synopsis :program-name program-name)
    (setf *configuration* (options-from-default-sources))
    (process-commandline-options
     :commandline     (list* program-name args)
     :version         (rsb-tools-bridge-system:version/list :commit? t)
     :update-synopsis (curry #'update-synopsis :program-name program-name)
     :return          (lambda () (return-from main)))
    (enable-swank-on-signal))

  (let* ((error-policy      (maybe-relay-to-thread
                             (process-error-handling-options)))
         (spec              (first (remainder)))
         (max-queued-events (getopt :long-name "max-queued-events")))
    (rsb.formatting:with-print-limits (*standard-output*)
      ;; Check bridge specification options.
      (unless spec
        (error "~@<Supply a bridge specification as remainder of the ~
                commandline.~@:>"))

      (with-logged-warnings
        (with-error-policy (error-policy)
          ;; Load IDLs as specified on the commandline.
          (process-idl-options :purpose '(:deserializer
                                          :packed-size :serializer))

          (let* ((spec    (rsb.tools.commands.bridge:parse-spec spec))
                 (command (make-command :bridge
                                        :spec              spec
                                        :max-queued-events max-queued-events)))
            (with-interactive-interrupt-exit ()
              (command-execute command :error-policy error-policy))))))))

;;;; main.lisp --- Entry point of the info tool.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.tools.info)

(defun update-synopsis (&key
                        (show :default))
  "Create and return a commandline option tree."
  (make-synopsis
   ;; Basic usage and specific options.
   :item    (make-text :contents "Describe the RSB system.")
   :item    (make-common-options :show show)
   ;;
   :item    (defgroup (:header "Display Options")
              (flag :long-name  "verbose"
                    :short-name "b"
                    :description
                    "Display all available information.")
              (flag :long-name  "configuration"
                    :short-name "f"
                    :description
                    "Display information regarding the default configuration?")
              (flag :long-name  "connectors"
                    :short-name "c"
                    :description
                    "Display information regarding available transport implementations?")
              (flag :long-name  "converters"
                    :short-name "v"
                    :description
                    "Display information regarding available converters?")
              (flag :long-name  "filters"
                    :short-name "i"
                    :description
                    "Display information regarding available filters?")
              (flag :long-name  "transforms"
                    :short-name "t"
                    :description
                    "Display information regarding available event transformations?")
              (flag :long-name  "event-processing"
                    :short-name "e"
                    :description
                    "Display information regarding available event processing strategies?")
              (flag :long-name  "participants"
                    :short-name "p"
                    :description
                    "Display information regarding available participant kinds?"))
   ;; Append RSB options.
   :item    (make-options
             :show? (or (eq show t)
                        (and (listp show) (member :rsb show))))))

(defun main (program-pathname args)
  "Entry point function of the cl-rsb-tools-info system."
  (update-synopsis)
  (setf *configuration* (options-from-default-sources))
  (process-commandline-options
   :commandline     (list* (concatenate
                            'string (namestring program-pathname) " info")
                           args)
   :version         (cl-rsb-tools-info-system:version/list :commit? t)
   :update-synopsis #'update-synopsis
   :return          (lambda () (return-from main)))
  (enable-swank-on-signal)

  (let+ ((stream   *standard-output*)
         (verbose? (getopt :long-name "verbose"))
         ((&flet make-initarg (name)
            (let* ((string    (string-downcase name))
                   (long-name (subseq string 0 (1- (length string)))))
              (when (getopt :long-name long-name)
                (list name t)))))
         (command (apply #'make-command :info
                         (append
                          (when verbose?
                            '(:all? t))
                          (mapcan #'make-initarg
                                  '(:version? :configuration? :connectors?
                                    :converters? :filters? :transforms?
                                    :event-processing? :participants?))))))
    (with-print-limits (stream)
      (with-logged-warnings
        (command-execute command)))))

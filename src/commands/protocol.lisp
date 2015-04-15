;;;; protocol.lisp --- Protocol provided by the commands module.
;;;;
;;;; Copyright (C) 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.tools.commands)

;;; Command protocol

(defgeneric command-execute (command &key error-policy)
  (:documentation
   "Execute COMMAND applying ERROR-POLICY in case of errors."))

;;; Command creation protocol

(defgeneric make-command (spec &rest args)
  (:documentation
   "Make and return a command instance according to SPEC and ARGS.

    SPEC must designate a command register in the `command'
    service."))

;; Default behavior

(defmethod make-command ((spec cons) &rest args)
  (check-type spec (cons keyword list) "a keyword followed by initargs")
  (apply #'make-command (first spec) (append (rest spec) args)))

(defmethod make-command ((spec t)
                         &rest args &key (service 'command) &allow-other-keys)
  (apply #'service-provider:make-provider service spec
         (remove-from-plist args :service)))

;;; Command service

(service-provider:define-service command
  (:documentation
   "Providers of this service define commands which can be executed by
    the rsb commandline tool."))

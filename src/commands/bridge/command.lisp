;;;; command.lisp --- Implementation of the bridge command.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.tools.commands.bridge)

;;; `bridge' command class

(defclass bridge (print-items:print-items-mixin)
  ((spec              :initarg  :spec
                      :type     bridge-description
                      :reader   bridge-spec
                      :documentation
                      "Stores the specification according to which the
                       bridge should be constructed and configured.")
   (self-filters      :initarg  :self-filters
                      :type     list
                      :reader   bridge-self-filters
                      :documentation
                      "Stores a list of filters necessary to prevent
                       forwarding cycles that would otherwise be
                       caused by events sent by the bridge and then
                       received by the bridge.")
   (max-queued-events :initarg  :max-queued-events
                      :type     (or null positive-integer)
                      :reader   bridge-max-queued-events
                      :initform 200
                      :documentation
                      "The maximum number of events which may be
                       queued for processing at any given time. Note
                       that choosing a large value can require a large
                       amount of memory."))
  (:default-initargs
   :spec (missing-required-initarg 'bridge :spec))
  (:documentation
   "Forward events from one part of a system to other parts of the system.

    When executed, the bridge command instantiates a bridge
    participant containing one or more connection participants. Each
    connection performs uni- or bidirectional forwarding of events
    between parts of the RSB system described by transports, scopes
    and filters."))

(service-provider:register-provider/class
 'rsb.tools.commands::command :bridge :class 'bridge)

(defmethod shared-initialize :around ((instance   bridge)
                                      (slot-names t)
                                      &rest args &key
                                      spec)
  ;; `check-description' signals (continuable) errors and warnings for
  ;; detected cycles and returns a list of necessary "self-filters" as
  ;; its second value.
  (let+ (((&values spec self-filters)
          (with-condition-translation
              (((error specification-error) :spec spec))
            (check-description (etypecase spec
                                 (string             (parse-spec spec))
                                 (bridge-description spec))))))
    (apply #'call-next-method
           instance slot-names
           :spec         spec
           :self-filters self-filters
           (remove-from-plist args :spec))))

(defmethod command-execute ((command bridge) &key error-policy)
  ;; `bridge-description->connection-list' transforms the list of
  ;; self-filter specifications of the form
  ;;
  ;;   (INPUT-DESCRIPTION . (OUTPUT-DESCRIPTION₁ OUTPUT-DESCRIPTION₂ …))
  ;;
  ;; into a list of specifications of the form
  ;;
  ;;   (INPUT-URI . ((OUTPUT-URI₁ . OUTPUT-ID₁) (OUTPUT-URI₂ . OUTPUT-ID₂) …))
  ;;
  ;; which the participant turns into a set of filters while creating
  ;; its child-participants.
  (let+ (((&structure-r/o bridge- spec self-filters max-queued-events) command)
         ((&values connections self-filters)
          (bridge-description->connection-list spec self-filters))
         (converters (rsb.tools.commands::ensure-fallback-converter)))
    (with-participant (bridge :bridge "/"
                              :converters        converters
                              :connections       connections
                              :self-filters      self-filters
                              :max-queued-events max-queued-events
                              :error-policy      error-policy)
      (rsb.patterns.bridge:pump-events bridge))))

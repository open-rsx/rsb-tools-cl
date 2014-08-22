;;;; info.lisp --- Entry point of the info tool.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.tools.commands)

(defclass info (output-stream-mixin)
  ((version?          :reader   info-version?
                      :accessor info-%version?
                      :initform nil
                      :documentation
                      "Display various pieces of version
                       information?")
   (configuration?    :reader   info-configuration?
                      :accessor info-%configuration?
                      :initform nil
                      :documentation
                      "Display information regarding the default
                       configuration?")
   (connectors?       :initform nil
                      :reader   info-connectors?
                      :accessor info-%connectors?
                      :documentation
                      "Display information regarding available
                       transport implementations?")
   (converters?       :initform nil
                      :reader   info-converters?
                      :accessor info-%converters?
                      :documentation
                      "Display information regarding available
                       converters?")
   (filters?          :initform nil
                      :reader   info-filters?
                      :accessor info-%filters?
                      :documentation
                      "Display information regarding available
                       filters?")
   (transforms?       :initform nil
                      :reader   info-transforms?
                      :accessor info-%transforms?
                      :documentation
                      "Display information regarding available
                       transforms?")
   (event-processing? :initform nil
                      :reader   info-event-processing?
                      :accessor info-%event-processing?
                      :documentation
                      "Display information regarding available event
                       processing strategies?")
   (participants?     :initform nil
                      :reader   info-participants?
                      :accessor info-%participants?
                      :documentation
                      "Display information regarding available
                       participant kinds?"))
  (:documentation
   "Display information about aspects of the RSB system."))

(service-provider:register-provider/class
 'command :info :class 'info)

(defmethod shared-initialize :after
    ((instance info) (slot-name t)
     &key
     (all?              nil all?-supplied?)
     (version?          nil version?-supplied?)
     (configuration?    nil configuration?-supplied?)
     (connectors?       nil connectors?-supplied?)
     (converters?       nil converters?-supplied?)
     (filters?          nil filters?-supplied?)
     (transforms?       nil transforms?-supplied?)
     (event-processing? nil event-processing?-supplied?)
     (participants?     nil participants?-supplied?))
  (macrolet ((do-option (name)
               (let ((supplied? (symbolicate name '#:-supplied?))
                     (accessor  (symbolicate '#:info-% name)))
                 `(cond
                    (all?-supplied?
                     (setf (,accessor instance) all?))
                    (,supplied?
                     (setf (,accessor instance) ,name))))))
    (do-option version?)
    (do-option configuration?)
    (do-option connectors?)
    (do-option converters?)
    (do-option filters?)
    (do-option transforms?)
    (do-option event-processing?)
    (do-option participants?)))

(defmethod command-execute ((command info) &key error-policy)
  (declare (ignore error-policy))

  (let+ (((&structure-r/o
           info- version? configuration? connectors? converters? filters?
           transforms? event-processing? participants?)
          command)
         (stream (command-stream command))
         (produced-output? nil)
         ((&flet format-service-providers (service)
            (format stream "~:[~;~2&~]~@(~A~)s~
                      ~&~2@T~@<~
                        ~:[<none>~;~:*~{+ ~<~@;~16A~@[ ~A~]~:>~^~@:_~}~]~
                      ~:>"
                    produced-output?
                    service
                    (mapcar (lambda (provider)
                              (list (service-provider:provider-name provider)
                                    (when-let ((documentation (documentation provider t)))
                                      (first-line-or-less documentation))))
                            (service-provider:service-providers service)))
            (setf produced-output? t))))

    (when version?
      (print-version nil stream)
      (setf produced-output? t))

    (when configuration?
      (format stream "~:[~;~2&~]Configuration~
                      ~&~2@T~@<~
                        ~{~48@<~(~{~A~^.~}~)~>: ~S~^~@:_~}~
                      ~:>"
              produced-output?
              (alist-plist *configuration*))
      (setf produced-output? t))

    (when connectors?
      (format stream "~:[~;~2&~]Connectors~
                      ~&~2@T~@<~
                        ~{+ ~<~@;~@{~A~*~}~:>~^~@:_~}~
                      ~:>"
              produced-output?
              (rsb.transport:transport-classes))
      (setf produced-output? t))

    (when converters?
      (format stream "~:[~;~2&~]Converters~
                      ~&~2@T~@<~
                        ~{+ ~<~@;~@{~A~*~}~:>~^~@:_~}~
                      ~:>"
              produced-output?
              (rsb.converter:converter-classes))
      (setf produced-output? t))

    (when filters?
      (format stream "~:[~;~2&~]Filters~&~2@T"
              produced-output?)
      (pprint-logical-block (stream (list nil))
        (print-filter-help stream))
      (setf produced-output? t))

    (when transforms?
      (format-service-providers 'rsb.transform::transform))

    (when event-processing?
      (format stream "~:[~;~2&~]Event Processors~
                      ~&~2@T~@<~
                        ~:[<none>~;~{+ ~<~@;~@{~A~*~}~:>~^~@:_~}~]~
                      ~:>"
              produced-output?
              (rsb.event-processing:processor-classes))
      (setf produced-output? t))

    (when participants?
      (format-service-providers 'rsb:participant))

    (when produced-output?
      (terpri stream))))

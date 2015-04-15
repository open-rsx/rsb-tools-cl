;;;; mixins.lisp --- Mixins class used by the commands module.
;;;;
;;;; Copyright (C) 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.tools.commands)

;;; Utility functions

(defun coerce-to-scope-or-uri (thing)
  (etypecase thing
    (string              (puri:parse-uri thing))
    ((or scope puri:uri) thing)))

(defun scope-or-uri-string (thing)
  (etypecase thing
    (puri:uri thing)
    (scope    (scope-string thing))))

;;; `source-mixin'

(defclass source-mixin ()
 ((uris :type     (or null (cons (or scope puri:uri))) ; list-of (or scope puri:uri)
        :reader   command-uris
        :accessor command-%uris
        :documentation
        "A list of source URIs from which events are received."))
  (:default-initargs
   :uris (missing-required-initarg 'source-mixin :uris))
  (:documentation
   "This class is intended to be mixed into command classes which
    receive events from a number of source URIs."))

(defmethod shared-initialize :after ((instance   source-mixin)
                                     (slot-names t)
                                     &key
                                     (uris nil uris-supplied?))
  (when uris-supplied?
    (setf (command-%uris instance) (mapcar #'coerce-to-scope-or-uri uris))))

(defmethod print-items:print-items append ((object source-mixin))
  (let ((strings (mapcar #'scope-or-uri-string (command-uris object))))
    `((:source-uris ,strings "~{~A~^, ~}"))))

(defmethod command-execute :before ((command source-mixin) &key error-policy)
  (declare (ignore error-policy))
  (log:info "~@<Using sources URI~P ~:*~{~S~^, ~}~@:>" (command-uris command)))

;;; `destination-mixin'

(defclass destination-mixin ()
  ((destination :type     (or scope puri:uri)
                :reader   command-destination
                :accessor command-%destination
                :documentation
                "A destination URI to which events are sent."))
  (:default-initargs
   :destination (missing-required-initarg 'destination-mixin :destination))
  (:documentation
   "This class is intended to be mixed into command classes which send
    events to a destination URI."))

(defmethod shared-initialize :after ((instance   destination-mixin)
                                     (slot-names t)
                                     &key
                                     (destination nil destination-supplied?))
  (when destination-supplied?
    (setf (command-%destination instance)
          (coerce-to-scope-or-uri destination))))

(defmethod print-items:print-items append ((object destination-mixin))
  `((:destination-uri ,(scope-or-uri-string (command-destination object)))))

(defmethod command-execute :before ((command destination-mixin) &key error-policy)
  (declare (ignore error-policy))
  (log:info "~@<Using destination URI ~S~@:>" (command-destination command)))

;;; `payload-literal-mixin'

(defclass payload-literal-mixin ()
  ((payload :initarg  :payload
            :reader   command-payload
            :accessor command-%payload
            :documentation
            "Stores the payload that should be used by the command."))
  (:documentation
   "This class is intended to be mixed into command classes that send
    events and thus need a way to specify the desired payload of these
    events."))

(defmethod shared-initialize :before ((instance   payload-literal-mixin)
                                      (slot-names t)
                                      &key
                                      (payload      nil payload-supplied?)
                                      (payload-spec nil payload-spec-supplied?))
  (cond
    ((and payload-supplied? payload-spec-supplied?)
     (incompatible-initargs 'payload-literal-mixin
                            :payload      payload
                            :payload-spec payload-spec))
    ((not (or payload-supplied? payload-spec-supplied?))
     (missing-required-initarg 'payload-literal-mixin
                               :payload-xor-payload-spec))))

(defmethod shared-initialize :after ((instance   payload-literal-mixin)
                                     (slot-names t)
                                     &key
                                     (payload-spec nil payload-spec-supplied?))
  (when payload-spec-supplied?
    (setf (command-%payload instance) (parse-payload-spec payload-spec))))

(defmethod command-execute :before ((command payload-literal-mixin) &key error-policy)
  (declare (ignore error-policy))
  (log:info "~@<Using payload ~S~@:>" (command-payload command)))

;;; `style-mixin'

(defclass style-mixin ()
  ((style         :initarg  :style
                  :reader   command-style
                  :accessor command-%style
                  :documentation
                  "A designator for the formatting style that should
                   be used by the command.")
   (style-service :allocation :class
                  :reader   command-style-service
                  :initform 'rsb.formatting::style
                  :documentation
                  "A designator for the formatting style service that
                   should be used by the command."))
  (:documentation
   "This class is intended to be mixed into command classes that have
    to use a style object for outputting information in different
    ways."))

(defmethod shared-initialize :before ((instance   style-mixin)
                                      (slot-names t)
                                      &key
                                      (style      nil style-supplied?)
                                      (style-spec nil style-spec-supplied?))
  (cond
    ((and style-supplied? style-spec-supplied?)
     (incompatible-initargs 'style-mixin
                            :style      style
                            :style-spec style-spec))
    ((not (or style-supplied? style-spec-supplied?))
     (missing-required-initarg 'style-mixin
                               :style-xor-style-spec))))

(defmethod shared-initialize :after
    ((instance   style-mixin)
     (slot-names t)
     &key
     (style-spec    nil                              style-spec-supplied?)
     (style-service (command-style-service instance)))
  (when style-spec-supplied?
    (setf (command-%style instance)
          (make-style (parse-instantiation-spec style-spec)
                      :service style-service))))

;;; `output-stream' mixin

(defclass output-stream-mixin ()
  ((stream :initarg  :stream
           :type     stream
           :reader   command-stream
           :accessor command-%stream
           :initform *standard-output*
           :documentation
           "Stream to which the command should write its output."))
  (:documentation
   "This class is intended to be mixed into command classes that
    output information on a stream."))

(defmethod shared-initialize :before ((instance   output-stream-mixin)
                                      (slot-names t)
                                      &key
                                      (stream      nil stream-supplied?)
                                      (stream-spec nil stream-spec-supplied?))
  (when (and stream-supplied? stream-spec-supplied?)
    (incompatible-initargs 'output-stream-mixin
                           :stream      stream
                           :stream-spec stream-spec)))

(defmethod shared-initialize :after ((instance   output-stream-mixin)
                                     (slot-names t)
                                     &key
                                     (stream-spec nil stream-spec-supplied?))
  (when stream-spec-supplied?
    (setf (command-%stream instance)
          (ecase stream-spec
            ((:stdout :standard-output) *standard-output*)
            ((:stderr :error-output)    *error-output*)))))

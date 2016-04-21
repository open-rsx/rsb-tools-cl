;;;; event-style-json.lisp --- Formatting things as JSON data.
;;;;
;;;; Copyright (C) 2012, 2014, 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.formatting)

;;; Basic infrastructure

(deftype pass-through-value ()
  "Values that can directly be converted into native JSON values."
  '(or real string boolean))

(deftype stringify-value ()
  "Values that have to be turned into strings for JSON conversion."
  '(or (and symbol (not boolean))
       event-id scope
       cons pathname
       uuid:uuid local-time:timestamp puri:uri))

(declaim (inline maybe-stringify-value
                 prepare-initarg-value-for-json))

(defun maybe-stringify-value (value)
  (typecase value
    ((and symbol (not boolean))
     (symbol-name value))
    (event-id
     (locally (declare (notinline maybe-stringify-value))
       (maybe-stringify-value (event-id->uuid value))))
    (scope
     (scope-string value))
    ((or cons
         pathname uuid:uuid local-time:timestamp puri:uri)
     (princ-to-string value))))

(defun prepare-initarg-value-for-json (value)
  (or (maybe-stringify-value value) value))

(defun make-json-peek-function (&optional next)
  (declare (type (or null function) next))
  (lambda (builder relation relation-args node)
    (typecase node
      (pass-through-value
       (values node 'architecture.builder-protocol.json:raw))
      (stringify-value
       (values (maybe-stringify-value node)
               'architecture.builder-protocol.json:raw))
      (t
       (if next
           (funcall next builder relation relation-args node)
           t)))))

(defun make-json-serializer (&key
                             kind-transform
                             peek-function)
  (let+ ((symbol-transform (architecture.builder-protocol.json:default-symbol-transform))
         (key-transform    (lambda (thing)
                             (typecase thing
                               (string thing)
                               (symbol (funcall symbol-transform thing))
                               (t      (maybe-stringify-value thing)))))
         ((&flet initarg-transform (key value)
            (values (funcall symbol-transform key)
                    (prepare-initarg-value-for-json value)))))
    (architecture.builder-protocol.json:make-serializer
     :kind-transform     kind-transform
     :initarg-transform  #'initarg-transform
     :relation-transform key-transform
     :member-transform   key-transform
     :peek-function      peek-function)))

;;; `style-json'

(defclass style-json (separator-mixin)
  ((builder            :initarg  :builder
                       :reader   style-builder
                       :initform t
                       :documentation
                       "Stores the builder that should be used to
                        serialize events and payloads to JSON.")
   (event-serializer   :accessor style-%event-serializer
                       :documentation
                       "Stores the serializer that should be used for
                        events.")
   (payload-serializer :accessor style-%payload-serializer
                       :documentation
                       "Stores the serializer that should be used for
                        payloads."))
  (:default-initargs
   :separator             nil
   :event-peek-function   (make-json-peek-function
                           (rsb.builder:universal-builder-for-event-data))
   :payload-peek-function (make-json-peek-function))
  (:documentation
   "Format events and payloads as JSON."))

(service-provider:register-provider/class
 'style :json :class 'style-json)

(defmethod shared-initialize :after
    ((instance   style-json)
     (slot-names t)
     &key
     (event-peek-function   nil event-peek-function-supplied?)
     (payload-peek-function nil payload-peek-function-supplied?))
  (when event-peek-function-supplied?
    (setf (style-%event-serializer instance)
          (make-json-serializer :peek-function event-peek-function)))
  (when payload-peek-function-supplied?
    (setf (style-%payload-serializer instance)
          (make-json-serializer :peek-function payload-peek-function))))

(defmethod rsb.ep:access? ((processor style-json)
                           (part      t)
                           (mode      (eql :read)))
  t)

(defmethod format-event ((thing  t)
                         (style  style-json)
                         (stream t)
                         &key)
  (let+ (((&structure-r/o style- builder %event-serializer) style))
    (architecture.builder-protocol:with-unbuilder (builder builder)
      (architecture.builder-protocol.json:serialize-using-serializer
       builder thing stream %event-serializer))))

(defmethod format-payload ((thing  t)
                           (style  style-json)
                           (stream t)
                           &key)
  (let+ (((&structure-r/o style- builder %payload-serializer) style))
    (architecture.builder-protocol:with-unbuilder (builder builder)
      (architecture.builder-protocol.json:serialize-using-serializer
       builder thing stream %payload-serializer))))

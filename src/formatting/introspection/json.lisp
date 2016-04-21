;;;; json.lisp --- JSON serialization of introspection information.
;;;;
;;;; Copyright (C) 2014, 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.formatting.introspection)

;;; `style-json'

(defclass style-json (database-mixin
                      delay-mixin)
  ((builder    :initarg  :builder
               :reader   style-builder
               :initform t
               :documentation
               "Stores the builder used to serialize introspection
                model objects to JSON.")
   (serializer :reader   style-%serializer
               :initform (rsb.formatting::make-json-serializer
                          :kind-transform (architecture.builder-protocol.json:default-kind-key-and-value-transform)
                          :peek-function  (rsb.formatting::make-json-peek-function))
               :documentation
               "Stores the serializer used to serialize introspection
                model objects to JSON."))
  (:documentation
   "Serialize introspection information to JSON."))

(service-provider:register-provider/class
 'style :json :class 'style-json)

(defmethod detach ((participant style-json)))

;; Introspection event => ignore.
(defmethod rsb.ep:handle ((sink style-json) (data list)))

;; Dummy event => serialize snapshot to JSON and return false to
;; indicate that the program should be terminated.
(defmethod rsb.formatting:format-event ((event  (eql :dummy))
                                        (style  style-json)
                                        (target t)
                                        &key)
  (let+ (((&structure-r/o style- (introspection database) builder %serializer)
          style))
    (with-database-lock (introspection)
      (let ((database (introspection-database introspection)))
        (architecture.builder-protocol:with-unbuilder (builder builder)
          (architecture.builder-protocol.json:serialize-using-serializer
           builder database target %serializer)))))
  nil)

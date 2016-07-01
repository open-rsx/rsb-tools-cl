;;;; introspection.lisp --- Serve introspection information over HTTP.
;;;;
;;;; Copyright (C) 2014, 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.tools.commands.web)

;;; `introspection-handler-mixin'

(defclass introspection-handler-mixin (handler-mixin)
  ((database :initarg  :database
             :reader   handler-database))
  (:default-initargs
   :database (missing-required-initarg 'introspection-handler-mixin :database))
  (:documentation
   "This class is intended to be mixed into handler classes that serve
    introspection information."))

;;; `introspection-snapshot-handler'

(defclass introspection-snapshot-handler (introspection-handler-mixin)
  ()
  (:metaclass closer-mop:funcallable-standard-class)
  (:documentation
   "Instances of this class serve JSON-serialized introspection
    information."))

(defun make-introspection-snapshot-handler (database)
  (values "/api/introspection/snapshot"
          (make-instance 'introspection-snapshot-handler :database database)))

(pushnew 'make-introspection-snapshot-handler *default-handlers*)

(defmethod rsb.ep:handle ((sink introspection-snapshot-handler)
                          (data hunchentoot:request))
  (setf (hunchentoot:header-out "Content-type") "application/json;charset=UTF-8")
  (let+ (((&structure-r/o handler- database) sink)
         (stream (flexi-streams:make-flexi-stream
                  (hunchentoot:send-headers) :external-format :utf-8)))
    (rsb.introspection:with-database-lock (database)
      (let ((tree (rsb.introspection::introspection-database database)))
        (architecture.builder-protocol.json:serialize-using-serializer
         t tree stream (default-json-serializer))))))

;;; Utilities

(defun default-json-serializer ()
  (rsb.formatting::make-json-serializer
   :kind-transform (architecture.builder-protocol.json:default-kind-key-and-value-transform)
   :peek-function  (rsb.formatting::make-json-peek-function)))

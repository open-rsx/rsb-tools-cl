;;;; introspection.lisp --- Serve introspection information over HTTP.
;;;;
;;;; Copyright (C) 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.tools.commands.web)

;;; `introspection-json-handler'

(defclass introspection-json-handler (function
                                      standard-object)
  ((style :accessor handler-%style
          :documentation
          "Stores the introspection formatting style for serializing
           the introspection data."))
  (:metaclass closer-mop:funcallable-standard-class)
  (:default-initargs
   :database (missing-required-initarg 'introspection-json-handler :database))
  (:documentation
   "Instances of this class serve JSON-serialized introspection
    information."))

(defmethod initialize-instance :after ((instance introspection-json-handler)
                                       &key
                                       database)
  (closer-mop:set-funcallable-instance-function
   instance (lambda () (rsb.ep:handle instance hunchentoot:*request*)))

  ;; Instantiate the JSON introspection formatting style and attach
  ;; the introspection database to it.
  (let+ (((&structure handler- (style %style)) instance))
    (setf style (rsb.formatting:make-style
                 :json :service 'rsb.formatting.introspection::style))
    (setf (rsb.formatting.introspection::style-database style) database)))

(defmethod rsb.ep:handle ((processor introspection-json-handler)
                          (data      hunchentoot:request))
  (setf (hunchentoot:header-out "Content-type") "application/json;charset=UTF-8")
  (let+ (((&structure-r/o handler- (style %style)) processor))
    (with-output-to-string (stream)
      (rsb.formatting:format-event :dummy style stream))))

;;;; event-style-json.lisp --- Formatting things as JSON data.
;;;;
;;;; Copyright (C) 2012, 2014, 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.formatting)

;;; `style-json'

(defclass style-json (separator-mixin)
  ()
  (:default-initargs
   :separator nil)
  (:documentation
   "Format events and payloads as JSON."))

(service-provider:register-provider/class
 'style :json :class 'style-json)

(defmethod rsb.ep:access? ((processor style-json)
                           (part      t)
                           (mode      (eql :read)))
  t)

(defmethod format-event ((thing  event)
                         (style  style-json)
                         (stream t)
                         &key)
  (json:encode-json thing stream))

(defmethod format-payload ((thing  t)
                           (style  style-json)
                           (stream t)
                           &key)
  (json:encode-json thing stream))

;;; Utilities

(defun encode-json-sequence (sequence &optional (stream json:*json-output*))
  (json::with-array (stream)
    (map nil (json::stream-array-member-encoder stream) sequence)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-members ((stream) &body body)
    (once-only (stream)
      `(json:with-object (,stream)
         (flet ((encode-member (name value)
                  (json:encode-object-member name value ,stream))
                (encode-member/alist (name value)
                  (json:as-object-member (name stream)
                    (json:encode-json-alist value stream)))
                (encode-member/sequence (name value)
                  (json:as-object-member (name stream)
                    (encode-json-sequence value stream))))
           (declare (dynamic-extent #'encode-member #'encode-member/alist
                                    #'encode-member/sequence)
                    (ignorable #'encode-member #'encode-member/alist
                               #'encode-member/sequence))
           ,@body)))))

;;; RSB-specific methods

(defmethod json:encode-json ((object event) &optional stream)
  (let+ (((&flet+ timestamp->string ((key . value))
            (cons key (princ-to-string value)))))
    (with-members (stream)
      (when-let ((number (event-sequence-number object)))
        (encode-member        "sequenceNumber" number))
      (when-let ((id (event-id object)))
        (encode-member        "id"             (princ-to-string id)))
      (encode-member          "scope"          (scope-string (event-scope object)))
      (when-let ((origin (event-origin object)))
        (encode-member        "origin"         (princ-to-string origin)))
      (encode-member          "method"             (event-method object))
      (encode-member/alist    "metaData"       (rsb:meta-data-alist object))
      (encode-member/alist    "timestamps"     (map 'list #'timestamp->string
                                                    (rsb:timestamp-alist object)))
      (encode-member/sequence "causes"         (map 'list (compose #'princ-to-string
                                                                   #'event-id->uuid)
                                                    (rsb:event-causes object)))
      (encode-member          "data"           (event-data object)))))

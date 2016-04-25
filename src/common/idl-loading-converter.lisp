;;;; idl-loading-converter.lisp --- Loading of IDL files at conversion time.
;;;;
;;;; Copyright (C) 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.tools.common)

(defclass idl-loading-converter (rsb.converter:caching-converter)
  ()
  (:documentation
   "Load data definitions on-the-fly, then perform actual conversion."))

(service-provider:register-provider/class
 'rsb.converter::converter :idl-loading :class 'idl-loading-converter)

;;; Deserialization

(defmethod rsb.converter:wire->domain? ((converter   idl-loading-converter)
                                        (wire-data   t)
                                        (wire-schema t))
  (let+ ((target (rsb.converter:converter-target converter))
         ((&flet ask-target ()
            (rsb.converter:wire->domain? target wire-data wire-schema))))
    (declare (dynamic-extent #'ask-target))
    (maybe-load-idl-for-wire-schema
     (symbol-name wire-schema) #'ask-target '(:deserializer))))

;;; Serialization

(defmethod rsb.converter:domain->wire? ((converter     idl-loading-converter)
                                        (domain-object t))
  (let+ ((target      (rsb.converter:converter-target converter))
         (class       (class-of domain-object))
         (wire-schema (protocol-buffer:descriptor-qualified-name
                       (pb:message-descriptor class)))
         ((&flet ask-target ()
            (rsb.converter:domain->wire? target domain-object))))
    (declare (dynamic-extent #'ask-target))
    (maybe-load-idl-for-wire-schema
     wire-schema #'ask-target '(:packed-size :serializer))))

;;; Utilities

(defun maybe-load-idl-for-wire-schema (wire-schema ask-target purpose)
  (declare (type string wire-schema)
           (type function ask-target))
  (let+ (((&flet load-definition ()
            (log:info "~@<Trying to load data type definition for ~
                       wire-schema ~S.~@:>"
                      wire-schema)
            (restart-case
                (find-and-load-idl wire-schema :proto :purpose purpose)
              (continue (&optional condition)
                :report (lambda (stream)
                          (format stream "~@<Ignore the error to load ~
                                          the data type definition for ~
                                          wire-schema ~S and ~
                                          continue.~@:>"
                                  wire-schema))
                (declare (ignore condition))
                nil))))
         (values (multiple-value-list (funcall ask-target))))
    (if (and values (first values))
        (values-list values)
        (and (load-definition)
             (funcall ask-target)))))

;;; Enabling

(defun maybe-ensure-idl-loading-converter (&key
                                           (converters (default-converters))
                                           (ensure?    *load-idl-on-demand?*))
  (let+ ((target    :protocol-buffer)
         (converter (rsb.converter:make-converter
                     :idl-loading :target target))
         ((&flet substitute-in-sequence (converters)
            (substitute converter target converters)))
         ((&labels substitute-in-alist (converters)
            (let* ((cell/old (assoc 'nibbles:octet-vector converters))
                   (cell/new (cons 'nibbles:octet-vector
                                   (substitute-converter (cdr cell/old)))))
              (substitute cell/new cell/old converters))))
         ((&labels substitute-in-caching-converter (converter)
            (let* ((target     (rsb.converter:converter-target converter))
                   (new-target (substitute-converter target)))
              (rsb.converter:make-converter :caching :target new-target))))
         ((&labels substitute-converter (converters)
            (etypecase converters
              (rsb.converter:caching-converter
               (substitute-in-caching-converter converters))
              ((cons cons)
               (substitute-in-alist converters))
              (sequence
               (substitute-in-sequence converters))))))
    (if ensure?
        (substitute-converter converters)
        converters)))

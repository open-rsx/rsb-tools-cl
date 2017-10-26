;;;; protocol-buffer-payload.lisp --- Protocol-buffer payload construction utilities.
;;;;
;;;; Copyright (C) 2015, 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.tools.common)

;;; Entry point

(defun build-protocol-buffer-message (descriptor-designator body)
  (let ((builder (make-instance 'message-builder
                                :descriptor descriptor-designator)))
    (bp:with-builder ((make-instance 'bp:top-down-forcing-builder
                                     :target builder))
      (esrap:parse 'serialization.protocol-buffer.parser.text-format:message body))
    (context-%message (message-builder-%context builder))))

;;; Builder
;;;
;;; Builds protocol buffer messages when driven by suitable
;;; `make-node' and `relate' calls.

(defun find-message-field (name descriptor)
  (or (find name (pb::message-desc-field descriptor)
            :key  #'pb:descriptor-name
            :test #'string=)
      (error "~@<No field ~S in message ~A.~@:>"
             name descriptor)))

(defun initarg->slot-name (class initarg)
  (c2mop:slot-definition-name
   (find initarg (c2mop:class-slots class)
         :test (lambda (initarg slot)
                 (member initarg (c2mop:slot-definition-initargs slot))))))

(defclass context ()
  ((descriptor :reader   context-descriptor
               :accessor context-%descriptor)
   (message    :reader   context-message
               :accessor context-%message)
   (field      :type     (or null string)
               :accessor context-field
               :initform nil)
   (next       :initarg  :next
               :reader   context-next
               :initform nil))
  (:default-initargs
   :descriptor (missing-required-initarg 'message-builder :descriptor)))

(defmethod shared-initialize :after ((instance   context)
                                     (slot-names t)
                                     &key
                                     descriptor
                                     message)
  (let* ((descriptor (etypecase descriptor
                       (string          (ensure-idl-loaded
                                         descriptor :purpose '(:packed-size
                                                               :serializer
                                                               :deserializer)))
                       (pb:message-desc descriptor)))
         (message    (or message
                         (make-instance (pb:descriptor-class descriptor)))))
    (setf (context-%descriptor instance) descriptor
          (context-%message    instance) message)))

(defclass message-builder ()
  ((context :accessor message-builder-%context)))

(defmethod shared-initialize :after ((instance   message-builder)
                                     (slot-names t)
                                     &key
                                     descriptor
                                     message)
  (setf (message-builder-%context instance)
        (make-instance 'context
                       :descriptor descriptor
                       :message    message)))

(defmethod bp:make-node ((builder message-builder) (kind (eql :message)) &key)
  (let+ (((&structure message-builder- (context %context)) builder)
         ((&structure-r/o context- descriptor field) context)
         ((&flet make-context ()
            (let ((descriptor (or (pb:field-type-descriptor
                                   (find-message-field field descriptor))
                                  (error "~@<Cannot assign message to ~
                                          non-message field ~A.~@:>"
                                         field))))
              (make-instance 'context
                             :descriptor descriptor
                             :next       context)))))
    (when field
      (setf context (make-context)))
    (context-%message context)))

(defmethod bp:finish-node ((builder message-builder) (kind (eql :message)) (node t))
  (let+ (((&structure message-builder- (context %context)) builder))
    (when-let ((next (context-next context)))
      (setf context next)))
  node)

(defmethod bp:make-node ((builder message-builder) (kind (eql :field))
                         &key name)
  (setf (context-field (message-builder-%context builder)) name)
  (list name))

(defmethod bp:relate ((builder message-builder) (relation (eql :field))
                      (left standard-object) (right list)
                      &key)
  (let+ (((name &rest values) right)
         ((&structure-r/o message-builder- (context %context)) builder)
         ((&structure-r/o context- (descriptor %descriptor) (message %message)) context)
         (field     (find-message-field name descriptor))
         (initarg   (make-keyword
                     (pbb:emit field '(:lisp-name :nested? nil))))
         (slot-name (initarg->slot-name (class-of message) initarg))
         (type      (pb:proto-type->lisp-type
                     (pbb::make-lisp-slot-type field)))
         ((&flet string->enum-value (string)
            (or (find (pb::->lisp-name string)
                      (rest (third (sb-ext:typexpand-1 type)))
                      :test #'string=)
                (error "~@<~S does not name a value of the enum field ~
                        ~A.~@:>"
                       string field)))))
    ;; Some ad-hoc conversions.
    (cond
      ((subtypep type 'float)
       (setf values (mapcar (rcurry #'coerce type) values)))
      ((and (subtypep type 'nibbles:octet-vector)
            (every (of-type 'string) values))
       (setf values (mapcar #'sb-ext:string-to-octets values)))
      ((and (subtypep type 'nibbles:octet-vector)
            (every (of-type 'integer) values))
       (setf values (list (coerce values 'nibbles:octet-vector))))
      ((pb:field-enum? field)
       (let ((value (first values)))
         (setf values (list (typecase value
                              (integer (pb:enum-symbol type value))
                              (string  (string->enum-value value))))))))
    ;; "Type checks"
    (unless (or (pb:field-repeated? field) (length= 1 values))
      (error "~@<Cannot assign ~D values ~{~S~^, ~} to non-repeated ~
              field ~A.~@:>"
             (length values) values field))
    (dolist (value values)
      (unless (typep value type)
        (error "~@<Value ~S is invalid for field ~A.~@:>"
               value field)))
    ;; Assign.
    (symbol-macrolet ((slot-value (slot-value message slot-name)))
      (cond
        ((not (pb:field-repeated? field))
         (setf slot-value (first values)))
        ((typep slot-value 'simple-array)
         (setf slot-value (concatenate
                           `(simple-array ,(array-element-type slot-value) (*))
                           slot-value values)))
        (t
         (dolist (value values)
           (vector-push-extend value slot-value)))))
    message))


(defmethod bp:relate ((builder message-builder) (relation t) (left t) (right t)
                      &key)
  (append left (list right)))

(defmethod bp:make-node ((builder message-builder) (kind (eql :literal))
                         &key value)
  value)

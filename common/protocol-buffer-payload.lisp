;;;; protocol-buffer-payload.lisp --- Protocol-buffer payload construction utilities.
;;;;
;;;; Copyright (C) 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.common)

;;; Entry point

(defun build-protocol-buffer-message (descriptor-designator body)
  (let ((builder (make-instance 'message-builder
                                :descriptor descriptor-designator)))
    (bp:with-builder ((make-instance 'bp:top-down-forcing-builder
                                     :target builder))
      (esrap:parse 'message body))
    (context-%message (message-builder-%context builder))))

;;; Rule macros

(defmacro defrule/s (name-and-options expression &body options)
  "Like `esrap:defule' but define additional rules named NAME/S and
   NAME/?S which respectively require/ allow EXPRESSION to be followed
   by skippable input.

   NAME-AND-OPTIONS can either just be a rule name or list of the form

     (NAME &key SKIPPABLE-RULE S? ?S? DEFINER)

   where SKIPPABLE-RULE names the rule used to parse skippable input
   in the NAME/S and NAME/?S variants. Defaults to `skippable'.

   S? and ?S? control which of the NAME/S and NAME/?S rules should be
   generated. Default is generating both.

   DEFINER is the name of the macro used to define \"main\"
   rule. Defaults to `esrap:defrule'."
  (let+ (((name
           &key
           (skippable-rule 'skippable)
           (s?             t)
           (?s?            t)
           (definer        'esrap:defrule))
          (ensure-list name-and-options))
         (name/s  (format-symbol *package* "~A/S" name))
         (name/?s (format-symbol *package* "~A/?S" name)))
    `(progn
       (,definer ,name
           ,expression
         ,@options)
       ,@(when s?
           `((esrap:defrule ,name/s
               (and ,name ,skippable-rule)
               (:function first))))
       ,@(when ?s?
           `((esrap:defrule ,name/?s
               (and ,name (esrap:? ,skippable-rule))
               (:function first)))))))

;;; Grammar
;;;
;;; Grammar for protocol buffer TextFormat. Mainly version 2 but with
;;; list syntax and simplifications from version 3.

(defrule/s message
    (or (and delimiter-</?s (* field/?s) delimiter->)
        (and delimiter-{/?s (* field/?s) delimiter-}))
  (:function second)
  (:lambda (fields)
    (bp:node* (:message)
      (* :field fields))))

(defrule/s field
    (or field/message field/scalar)
  (:destructure (name colon values)
    (declare (ignore colon))
    (bp:node* (:field :name name)
      (* :value values))))

(esrap:defrule field/message
    (and identifier/?s (esrap:? delimiter-colon/?s)
         (or list/message (and message))))

(esrap:defrule field/scalar
    (and identifier/?s delimiter-colon/?s
         (or list/scalar (and value/scalar))))

(defrule/s identifier ; TODO reuse rule from other grammar
    (and (or (esrap:character-ranges (#\a #\z) (#\A #\Z)) #\_)
         (* (or (esrap:character-ranges (#\0 #\9) (#\a #\z) (#\A #\Z)) #\_ #\+ #\-)))
  (:text t))

(macrolet
    ((define-list (suffix
                   &key
                   (value-name (symbolicate '#:value/ suffix '#:/?s)))
       (let ((list-name   (symbolicate '#:list/ suffix))
             (values-name (symbolicate '#:values/ suffix '#:/?s)))
         `(progn
            (esrap:defrule ,list-name
                (and delimiter-[/?s ,values-name delimiter-])
              (:function second))

            (esrap:defrule ,values-name
                (and ,value-name (* (and delimiter-comma/?s ,value-name)))
              (:destructure (first rest)
                (when first (list* first (mapcar #'second rest)))))))))
  (define-list scalar)
  (define-list message :value-name message/?s))

(defrule/s value/scalar
    (or string float integer bool)
  (:lambda (value)
    (bp:node* (:literal :value value))))

(esrap:defrule sign
    (or #\- #\+)
  (:lambda (sign)
    (switch (sign :test #'string=)
      (#\- -1)
      (#\+  1))))

(defun parse-integer-literal (radix text position end)
  (let+ (((&values value new-position)
          (parse-integer text :radix radix
                         :start position :end end :junk-allowed t)))
    (if value
        (values value new-position)
        (values nil position))))

(defun parse-hex-literal (text position end)
  (parse-integer-literal 16 text position end))

(defun parse-decimal-literal (text position end)
  (parse-integer-literal 10 text position end))

(defun parse-octal-literal (text position end)
  (parse-integer-literal 8 text position end))

(esrap:defrule integer
  #'parse-decimal-literal)

(defun decimal-digit-char? (character)
  (digit-char-p character 10))

(esrap:defrule float-decimals
    (and #\. (* (decimal-digit-char? character)))
  (:function second)
  (:text t)
  (:lambda (digits)
    (unless (emptyp digits)
      (/ (parse-integer digits) (expt 10 (length digits))))))

(esrap:defrule float-scientific
    (and (esrap:? #\.) (esrap:~ #\e) integer)
  (:function third)
  (:lambda (digits)
    (expt 10 digits)))

(esrap:defrule float
    (and (esrap:? sign) (esrap:! sign)
         (or (and (esrap:? integer) float-decimals           (esrap:? float-scientific))
             (and integer           (esrap:? float-decimals) float-scientific)))
  (:destructure (sign nosign1 (digits decimals scientific))
    (declare (ignore nosign1))
    (let ((number (* (or sign 1)
                     (+ (or digits 0) (or decimals 0))
                     (or scientific 1))))
      (float number 1.0f0))))

(esrap:defrule bool
  (or true false))

(esrap:defrule true
  (or "true" "t" "1")
  (:lambda (value)
    (declare (ignore value))
    t))

(esrap:defrule false
  (or "false" "f" "0")
  (:lambda (value)
    (declare (ignore value))
    nil))

(esrap:defrule string
    (or (and #\" (* string-element/double-quote) #\")
        (and #\' (* string-element/single-quote) #\'))
  (:function second)
  (:text t))

(macrolet ((define-string-element (name delimiter)
             `(esrap:defrule ,name
                  (or (and #\\   (or hex-escape octal-escape #\\ #\n #\t ,delimiter))
                      (and (and) (not (or #\\ ,delimiter))))
                (:function second))))
  (define-string-element string-element/double-quote #\")
  (define-string-element string-element/single-quote #\'))

(esrap:defrule hex-escape
    (and #\x #'parse-octal-literal)
  (:function second)
  (:function code-char))

(esrap:defrule octal-escape
    #'parse-octal-literal
  (:function code-char))

;;; Some lexical stuff

(esrap:defrule whitespace
  (or #\Space #\Tab #\Newline))

(esrap:defrule skippable
  (+ whitespace)
  (:constant nil))

(macrolet ((define-delimiter (name character)
             (let ((rule-name (symbolicate '#:delimiter- name)))
               `(defrule/s ,rule-name ,character))))
  (define-delimiter colon #\:)
  (define-delimiter comma #\,)
  (define-delimiter <     #\<)
  (define-delimiter >     #\>)
  (define-delimiter {     #\{)
  (define-delimiter }     #\})
  (define-delimiter [     #\[)
  (define-delimiter ]     #\]))

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
                       (string          (pb:find-descriptor descriptor))
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

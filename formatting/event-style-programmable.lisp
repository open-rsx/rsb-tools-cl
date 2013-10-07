;;;; event-style-progammable.lisp --- A programmable formatting style.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsb.formatting)

;;; Compilation protocol

(defgeneric compile-code (style code bindings)
  (:documentation
   "Compile CODE using BINDINGS for use with STYLE and return the
resulting compiled function."))

;;; Utility functions

(defmacro with-interpol-syntax (() &body body)
  "Execute BODY with interpol syntax enabled."
  `(unwind-protect
        (progn
          (interpol:enable-interpol-syntax)
          ,@body)
     (interpol:disable-interpol-syntax)))

(defvar *style-programmable-default-bindings*
  `((sequence-number (event-sequence-number event))
    (id              (princ-to-string (event-id event)))
    (scope           (scope-string (event-scope event)))
    (origin          (event-origin event))
    (data            (event-data event))
    ,@(iter (for timestamp in '(create send receive deliver))
            (collect `(,timestamp (timestamp event ,(make-keyword timestamp))))
            (collect `(,(symbolicate timestamp "-UNIX")
                       (timestamp->unix
                        (timestamp event ,(make-keyword timestamp)))))
            (collect `(,(symbolicate timestamp "-UNIX/NSEC")
                       (timestamp->unix/nsec
                        (timestamp event ,(make-keyword timestamp))))))
    (causes/event-id (event-causes event))
    (causes/uuid     (map 'list #'event-id->uuid (event-causes event)))
    (skip-event      (throw 'skip-event nil)))
  "A list of default bindings available in instances of
`style-programmable' and subclasses. Entries are like `cl:let'
bindings, that is

  (VARIABLE FORM)

where FORM is evaluated to produce the value of VARIABLE.")

(defclass style-programmable ()
  ((bindings :initarg  :bindings
             :type     list
             :accessor style-bindings
             :accessor %style-bindings
             :initform *style-programmable-default-bindings*
             :documentation
             "Stores the bindings available in the output format
specification.")
   (lambda   :type     function
             :accessor %style-lambda
             :documentation
             "Stores the compiled output formatting function for the
style instance.")
   (code     :type     list
             :accessor style-code
             :accessor %style-code
             :documentation
             "Stores the code producing the output of the style
instance."))
  (:default-initargs
   :code (missing-required-initarg 'style-programmable :code))
  (:documentation
   "This formatting style produces its output by executing a supplied
code. The supplied code can rely on `stream' to be bound to a
character output stream which it should use for producing the output.

By default, the following bindings are available:
"))

(defmethod shared-initialize :after ((instance   style-programmable)
                                     (slot-names t)
                                     &key
                                     (bindings nil bindings-supplied?)
                                     (code     nil code-supplied?))
  (when bindings-supplied?
    (check-type bindings list "a list of items of the form (SYMBOL FORM)"))

  (when code-supplied?
    (setf (style-code instance) code)))

(defmethod (setf style-code) :before ((new-value t)
                                      (style     style-programmable))
  (let+ (((&accessors (lambda   %style-lambda)
                      (bindings %style-bindings)) style))
    (setf lambda (compile-code style new-value bindings))))

(defmethod (setf style-bindings) :before ((new-value list)
                                          (style     style-programmable))
  (let+ (((&accessors (lambda %style-lambda)
                      (code   %style-code)) style))
    (setf lambda (compile-code style code new-value))))

(defmethod compile-code ((style    style-programmable)
                         (code     t)
                         (bindings list))
  (log1 :info style "Compiling code ~S" code)
  ;; Try to compile CODE collecting and subsequently muffling all
  ;; errors and warnings since we do not want to leak these to the
  ;; caller.
  (let+ ((conditions '())
         ((&flet wrap-code (code)
            `(lambda (event stream)
               (declare (ignorable event stream))
               (flet ((timestamp->unix (timestamp)
                        (local-time:timestamp-to-unix timestamp))
                      (timestamp->unix/nsec (timestamp)
                        (+ (* (expt 10 9)
                              (local-time:timestamp-to-unix timestamp))
                           (local-time:nsec-of timestamp))))
                 (declare (ignorable #'timestamp->unix
                                     #'timestamp->unix/nsec))
                 (symbol-macrolet (,@bindings)
                   (catch 'skip-event ,@code))))))
         ((&values function nil failed?)
          (block compile
            (handler-bind
                (#+sbcl
                 (sb-ext:compiler-note
                   #'(lambda (condition)
                       (log1 :info "Compiler said: ~A" condition)
                       (muffle-warning)))
                 (style-warning
                   #'(lambda (condition)
                       (log1 :warn "Compiler said: ~A" condition)
                       (muffle-warning)))
                 ((or error warning)
                   #'(lambda (condition)
                       (push condition conditions)
                       (if (find-restart 'muffle-warning)
                           (muffle-warning condition)
                           (return-from
                            compile (values nil nil t))))))
              (with-compilation-unit (:override t)
                (compile nil (wrap-code code)))))))
    (when (or failed? conditions)
      (format-code-error code
                         "~@<Failed to compile.~@[ Compiler said: ~
~:{~&+_~@<~@;~A~:>~}~]~@:>"
                         (mapcar #'list conditions)))
function))

(defmethod format-event ((event  event)
                         (style  style-programmable)
                         (stream stream)
                         &key &allow-other-keys)
  (let+ (((&accessors-r/o (bindings style-bindings)
                          (code     style-code)) style))
   (handler-bind
       (((and error (not stream-error))
          (lambda (condition)
            (format-code-error code
                               "~@<Failed to format event ~A using ~
specified bindings ~_~{~2T~{~24A -> ~A~_~}~}and code ~_~2T~S~_: ~
~A~@:>"
                               event bindings code condition))))
     (funcall (%style-lambda style) event stream))))

(defmethod print-object ((object style-programmable) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (if (slot-boundp object 'code)
        (let+ (((&accessors-r/o (code     style-code)
                                (bindings style-bindings)) object)
               (*print-length* (or *print-length* 3)))
          (format stream "~:[<no code>~;~:*~A~] " code)
          (%print-bindings bindings stream))
        (format stream "<not initialized>"))))

;;; Script-based style

(defmethod find-style-class ((spec (eql :programmable/script)))
  (find-class 'style-programmable/script))

(defclass style-programmable/script (style-programmable)
  ((code :reader   style-script))
  (:default-initargs
   :code   nil
   :script (missing-required-initarg
            'style-programmable/script :script))
  (:documentation
   "This formatting style produces its output by executing a supplied
script which can take the forms of a pathname, designating a file, a
stream, a string or any other Lisp object. The code can rely on
`stream' to be bound to a character output stream which it should use
for producing the output.

By default, the following bindings are available:
"))

(defmethod shared-initialize :after ((instance   style-programmable/script)
                                     (slot-names t)
                                     &key
                                     code
                                     (script nil script-supplied?))
  (when code
    (error "~@<The initarg ~S cannot be supplied; ~S has to be used.~@:>"
           :code :script))

  (when script-supplied?
    (setf (style-script instance) script)))

(defmethod (setf style-script) ((new-value list)
                                (style     style-programmable/script))
  (setf (style-code style) new-value))

(defmethod (setf style-script) ((new-value stream)
                                (style     style-programmable/script))
  (setf (style-script style)
        (with-condition-translation
            (((error format-code-read-error)
              :code new-value))
          (let ((*package* #.*package*))
            (iter (for token in-stream new-value)
                  (collect token)))))
  new-value)

(defmethod (setf style-script) ((new-value string)
                                (style     style-programmable/script))
  (with-input-from-string (stream new-value)
    (setf (style-script style) stream))
  new-value)

(defmethod (setf style-script) ((new-value pathname)
                                (style     style-programmable/script))
  (with-condition-translation
      ((((and error (not format-code-error))
         format-code-read-error)
        :code new-value))
    (with-input-from-file (stream new-value)
      (setf (style-script style) stream)))
  new-value)

;;; Template-based style

(defmethod find-style-class ((spec (eql :programmable/template)))
  (find-class 'style-programmable/template))

(defclass style-programmable/template (style-programmable)
  ((template :type     string
             :accessor style-template
             :writer   (setf %style-template)
             :documentation
             "Stores the template which is used for producing the
output of the style."))
  (:default-initargs
   :code     nil
   :template (missing-required-initarg
              'style-programmable/template :template))
  (:documentation
   "This formatting style produces its output by applying a template
specification to individual events. In the template specification,
event properties can be accessed using a syntax of the form
${PROPERTY} or @{PROPERTY} for \"direct\" expansion and \"spliced\"
expansion respectively. In addition, interpolations like named unicode
characters etc. as described in http://weitz.de/cl-interpol/ are
supported.

By default, the following PROPERTY names are available:
"))

(defmethod shared-initialize :after ((instance   style-programmable/template)
                                     (slot-names t)
                                     &key
                                     code
                                     (template nil template-supplied?))
  (when code
    (error "~@<The initarg ~S cannot be supplied; ~S has to be used.~@:>"
           :code :template))

  (when template-supplied?
    (setf (style-template instance) template)))

(defmethod (setf style-template) :before ((new-value string)
                                          (style     style-programmable/template))
  (let ((form (with-condition-translation
                  (((error format-code-read-error)
                    :code new-value))
                (let ((*package* #.*package*))
                  (with-interpol-syntax ()
                    (read-from-string
                     (format nil "#?\"~A\"" new-value)))))))
    (setf (style-code style) `((princ ,form stream)))))

(defmethod (setf style-template) ((new-value t)
                                  (style     style-programmable/template))
  (check-type new-value string)
  (setf (%style-template style) new-value))

(defmethod (setf style-template) ((new-value stream)
                                  (style     style-programmable/template))
  (setf (style-template style) (read-stream-content-into-string new-value))
  new-value)

(defmethod (setf style-template) ((new-value pathname)
                                  (style     style-programmable/template))
  (setf (style-template style)
        (with-condition-translation
            (((error format-code-read-error)
              :code new-value))
          (read-file-into-string new-value)))
  new-value)

(defmethod format-event ((event  event)
                         (style  style-programmable/template)
                         (stream stream)
                         &key &allow-other-keys)
  (handler-bind
      (((and error (not stream-error))
        (lambda (condition)
          (error "~@<Failed to format event ~A using specified ~
bindings ~_~{~2T~{~16A -> ~A~_~}~}and template ~_~2T~S~_: ~A~@:>"
                 event
                 (style-bindings style) (style-template style)
                 condition))))
    (funcall (%style-lambda style) event stream)))

(defmethod print-object ((object style-programmable/template) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (if (slot-boundp object 'template)
        (let+ (((&accessors-r/o (template style-template)
                                (bindings style-bindings)) object)
               (length (length template)))
          (format stream "\"~A~:[~;…~]\" "
                  (subseq template 0 (min 8 length)) (> length 8))
          (%print-bindings bindings stream))
        (call-next-method))))

;;; Utility functions

(defun %print-bindings (bindings stream)
  (format stream "(~D~:[~;*~])"
          (length bindings)
          (eq bindings *style-programmable-default-bindings*)))

(iter (for class in '(style-programmable
                      style-programmable/script
                      style-programmable/template))
      (setf (documentation class 'type)
            (format nil "~A~{+ ~(~A~)~^~%~}"
                    (documentation class 'type)
                    (mapcar #'first *style-programmable-default-bindings*))))

;; Local Variables:
;; coding: utf-8
;; End:

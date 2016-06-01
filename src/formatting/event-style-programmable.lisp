;;;; event-style-progammable.lisp --- A programmable formatting style.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.formatting)

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
  `((count           %count)
    (sequence-number (event-sequence-number %event))
    (id              (princ-to-string (event-id %event)))
    (scope           (scope-string (event-scope %event)))
    (origin          (event-origin %event))
    (method          (event-method %event))
    (data            (event-data %event))
    ,@(iter (for timestamp in '(create send receive deliver))
            (collect `(,timestamp (timestamp %event ,(make-keyword timestamp))))
            (collect `(,(symbolicate timestamp "-UNIX")
                       (timestamp->unix
                        (timestamp %event ,(make-keyword timestamp)))))
            (collect `(,(symbolicate timestamp "-UNIX/NSEC")
                       (timestamp->unix/nsec
                        (timestamp %event ,(make-keyword timestamp))))))
    (causes/event-id (event-causes %event))
    (causes/uuid     (map 'list #'event-id->uuid (event-causes %event)))
    (skip-event      (throw 'skip-event nil)))
  "A list of default bindings available in instances of
   `style-programmable' and subclasses. Entries are like `cl:let'
   bindings, that is

     (VARIABLE FORM)

   where FORM is evaluated to produce the value of VARIABLE.")

(defvar *style-programmable-default-binding-access-info*
  '((:scope           . (event scope))
    (:origin          . (event origin id))
    (:sequence-number . (event sequence-number id))
    (:method          . (event method))
    (:data            . (event data))
    (:timestamp       . (event create  create-unix  create-unix/nsec
                               send    send-unix    send-unix/nsec
                               receive receive-unix receive-unix/nsec
                               deliver deliver-unix deliver-unix/nsec))
    (:meta-data       . (event))
    (:cause           . (event causes/event-id causes/uuid))))

(defclass style-programmable ()
  ((bindings      :initarg  :bindings
                  :type     list
                  :accessor style-bindings
                  :accessor style-%bindings
                  :initform *style-programmable-default-bindings*
                  :documentation
                  "Stores the bindings available in the output format
                   specification.")
   (lambda        :type     function
                  :accessor style-%lambda
                  :documentation
                  "Stores the compiled output formatting function for
                   the style instance.")
   (code          :type     list
                  :accessor style-code
                  :accessor style-%code
                  :documentation
                  "Stores the code producing the output of the style
                   instance.")
   (used-bindings :accessor style-%used-bindings
                  :initform '()
                  :documentation
                  "Stores a list of names of bindings used in the
                   code."))
  (:default-initargs
   :code (missing-required-initarg 'style-programmable :code))
  (:documentation
   "This formatting style produces its output by executing a supplied
    code. The supplied code can rely on `stream' to be bound to a
    character output stream which it should use for producing the
    output.

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

(flet ((recompile (style code bindings)
         (setf (values (style-%lambda style) (style-%used-bindings style))
               (compile-code style code bindings))))

  (defmethod (setf style-code) :before ((new-value t)
                                        (style     style-programmable))
    (recompile style new-value (style-%bindings style))
    new-value)

  (defmethod (setf style-bindings) :before ((new-value list)
                                            (style     style-programmable))
    (recompile style (style-%code style) new-value)
    new-value))

(defmethod rsb.ep:access? ((processor style-programmable)
                           (part      t)
                           (mode      (eql :read)))
  (when-let ((bindings (cdr (assoc part *style-programmable-default-binding-access-info*))))
    (log:debug "~@<Checking ~S access to ~S against bindings ~S~@:>"
               mode part bindings)
    (when-let ((used (intersection bindings (style-%used-bindings processor))))
      (log:info "~@<Used binding~P ~{~S~^, ~} require ~S access to ~S~@:>"
                (length used) used mode part)
      t)))

(define-condition binding-use (condition)
  ((name :initarg :name
         :reader  binding-use-name)))

(defmethod compile-code ((style    style-programmable)
                         (code     t)
                         (bindings list))
  (log:info "~@<~A compiles code ~S~@:>" style code)
  ;; Try to compile CODE collecting and subsequently muffling all
  ;; errors and warnings since we do not want to leak these to the
  ;; caller.
  (let+ ((used-bindings '())
         (conditions    '())
         ((&flet+ instrument-binding ((name value))
            `(,name (note-binding-use ,name ,value))))
         ((&flet wrap-code (code)
            `(lambda ()
               (let ((%count -1))
                 (lambda (%event stream)
                   (declare (ignorable %event stream))
                   (incf %count)
                   (flet ((timestamp->unix (timestamp)
                            (local-time:timestamp-to-unix timestamp))
                          (timestamp->unix/nsec (timestamp)
                            (+ (* (expt 10 9)
                                  (local-time:timestamp-to-unix timestamp))
                               (local-time:nsec-of timestamp))))
                     (declare (ignorable #'timestamp->unix
                                         #'timestamp->unix/nsec))
                     (macrolet ((note-binding-use (name value)
                                  (signal 'binding-use :name name)
                                  value))
                       (symbol-macrolet
                           (,@(mapcar #'instrument-binding
                                      (list* '(event %event) bindings)))
                         (catch 'skip-event ,@code)))))))))
         ((&values function nil failed?)
          (block compile
            (handler-bind
                ((binding-use
                  (lambda (condition)
                    (let ((name (binding-use-name condition)))
                      (log:debug "~@<Saw use of binding ~S~@:>" name)
                      (pushnew name used-bindings))))
                 ((or style-warning #+sbcl sb-ext:compiler-note)
                  (lambda (condition)
                    (log:warn "~@<Compiler said: ~A~@:>" condition)
                    (muffle-warning)))
                 ((or error warning)
                  (lambda (condition)
                    (push condition conditions)
                    (if (find-restart 'muffle-warning)
                        (muffle-warning condition)
                        (return-from
                         compile (values nil nil t))))))
              (funcall
               (with-compilation-unit (:override t)
                 (compile nil (wrap-code code))))))))
    (when (or failed? conditions)
      (format-code-error code
                         "~@<Failed to compile.~@[ Compiler said: ~
                          ~:{~&+_~@<~@;~A~:>~}~]~@:>"
                         (mapcar #'list conditions)))
    (log:info "~@<Used bindings: ~:[none~;~:*~{~S~^, ~}~]~@:>" used-bindings)
    (values function used-bindings)))

(defmethod format-event ((event  event)
                         (style  style-programmable)
                         (stream stream)
                         &key)
  (let+ (((&structure-r/o style- bindings code) style))
   (handler-bind
       (((and error (not stream-error))
          (lambda (condition)
            (format-code-error code
                               "~@<Failed to format event ~A using ~
                                specified bindings ~_~{~2T~{~24A -> ~
                                ~A~_~}~}and code ~_~2T~S~_: ~A~@:>"
                               event bindings code condition))))
     (funcall (style-%lambda style) event stream))))

(defmethod print-object ((object style-programmable) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (if (slot-boundp object 'code)
        (let+ (((&structure-r/o style- code bindings) object)
               (*print-length* (or *print-length* 3)))
          (format stream "~:[<no code>~;~:*~A~] " code)
          (%print-bindings bindings stream))
        (format stream "<not initialized>"))))

;;; Script-based style

(defclass style-programmable/script (style-programmable)
  ((code :reader   style-script))
  (:default-initargs
   :code   nil
   :script (missing-required-initarg
            'style-programmable/script :script))
  (:documentation
   "This formatting style produces its output by executing a supplied
    script which can take the forms of a pathname, designating a file,
    a stream, a string or any other Lisp object. The code can rely on
    `stream' to be bound to a character output stream which it should
    use for producing the output.

    By default, the following bindings are available:
"))

(service-provider:register-provider/class
 'style :programmable/script :class 'style-programmable/script)

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

(defclass style-programmable/template (style-programmable)
  ((template :type     string
             :reader   style-template
             :writer   (setf style-%template)
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
    ${PROPERTY} or @{PROPERTY} for \"direct\" expansion and
    \"spliced\" expansion respectively. In addition, interpolations
    like named unicode characters etc. as described in
    http://weitz.de/cl-interpol/ are supported.

    By default, the following PROPERTY names are available:
"))

(service-provider:register-provider/class
 'style :programmable/template :class 'style-programmable/template)

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

(defmethod (setf style-template) ((new-value string)
                                  (style     style-programmable/template))
  (setf (style-%template style) new-value))

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
                         &key)
  (handler-bind
      (((and error (not stream-error))
        (lambda (condition)
          (error "~@<Failed to format event ~A using specified ~
                  bindings ~_~{~2T~{~16A -> ~A~_~}~}and template ~
                  ~_~2T~S~_: ~A~@:>"
                 event
                 (style-bindings style) (style-template style)
                 condition))))
    (funcall (style-%lambda style) event stream)))

(defmethod print-object ((object style-programmable/template) stream)
  (if (slot-boundp object 'template)
      (print-unreadable-object (object stream :type t :identity t)
        (let+ (((&structure-r/o style- template bindings) object)
               (length (length template)))
          (format stream "\"~A~:[~;…~]\" "
                  (subseq template 0 (min 8 length)) (> length 8))
          (%print-bindings bindings stream)))
      (call-next-method)))

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

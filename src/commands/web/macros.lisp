;;;; macros.lisp --- Macros used in the commands.web module.
;;;;
;;;; Copyright (C) 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.tools.commands.web)

;;; Macros related to query argument processing

(defun %transform-argument (parameter value transform)
  (with-condition-translation (((error argument-parse-error)
                                :parameter parameter
                                :raw-value value))
    (funcall transform value)))

(defun %type-check-argument (parameter value type)
  (unless (typep value type)
    (error 'argument-type-error
           :parameter     parameter
           :datum         value
           :expected-type type))
  value)

(defun parse-argument-spec (spec optional?)
  (let+ (((var
           &key
           (name         (string-downcase var))
           type
           transform
           (default      nil                   default-supplied?)
           documentation)
          (ensure-list spec)))
    (check-type var  symbol)
    (check-type name string)
    (when (and default-supplied? (not optional?))
      (error "~@<Required parameter ~S cannot have a default ~
              value.~@:>"
             name))
    (values var name type transform default default-supplied? optional?
            documentation)))

(defun map-parsed-arguments-specs (function specs)
  (let+ (((required &optional optional)
          (split-sequence:split-sequence '&optional specs))
         ((&flet do-spec (spec optional?)
            (multiple-value-call function
              (parse-argument-spec spec optional?)))))
    (append (mapcar (rcurry #'do-spec nil) required)
            (mapcar (rcurry #'do-spec t)   optional))))

(defmacro with-arguments ((request &rest parameters) &body body)
  "Execute BODY with bindings of REQUEST arguments according to PARAMETERS.

   PARAMETERS is a list of entries of one of the forms

     &optional
     VARIABLE-NAME
     (VARIABLE-NAME &key NAME TYPE TRANSFORM DEFAULT DOCUMENTATION)

   where VARIABLE-NAME is the name of the variable that should be
   bound to the value of the request parameter named NAME.

   TRANSFORM, if supplied, must evaluate to a function of one argument
   which is called with the raw value of the request argument and
   returns transformed (e.g. parsed) value. An `argument-parse-error'
   is signaled if TRANSFORM signals an error.

   TYPE, if supplied, is a type specifier designating the type
   of (potentially TRANSFORMed) values of the argument. An
   `argument-type-error' is signaled if a argument value is not of
   this type.

   DEFAULT can only be supplied for optional parameters. If a request
   does not contain a value for an optional parameter, its DEFAULT is
   evaluated to produce a value to be used instead.

   DOCUMENTATION can be used to attach a documentation string to the
   parameter.

   The &optional entry in PARAMETERS marks the entries following it as
   optional."
  (let+ (((&flet make-binding (var name type transform default default-supplied?
                               optional? documentation)
            (declare (ignore default-supplied? documentation))
            (let+ (((&flet maybe-transform (form)
                      (if transform
                          `(%transform-argument ,name ,form ,transform)
                          form)))
                   ((&flet maybe-type-check (form)
                      (if type
                          `(%type-check-argument ,name ,form ',type)
                          form))))
              `(,var (if-let ((value (hunchentoot:parameter ,name ,request)))
                       ,(maybe-type-check (maybe-transform 'value))
                       ,(if optional?
                            default
                            `(error 'missing-required-argument
                                    :parameter ,name))))))))
    `(let ,(map-parsed-arguments-specs #'make-binding parameters)
       ,@body)))

;;; API-related macros

(defgeneric api-reply (stream type thing))

(defmethod api-reply ((stream stream) (type (eql :json)) (thing t))
  (cl-json:encode-json thing stream))

(flet ((add-error-member (stream error)
         (let ((description (let ((*print-circle* t))
                              (princ-to-string error))))
           (cl-json:encode-object-member "error" description stream))))

  (defmethod api-reply ((stream stream) (type (eql :json)) (thing function))
    (cl-json:with-object (stream)
      (handler-case
          (cl-json:as-object-member ("data" stream)
            (funcall thing stream))
        (error (condition)
          (add-error-member stream condition)))))

  (defmethod api-reply ((stream stream) (type (eql :json)) (thing error))
    (cl-json:with-object (stream)
      (add-error-member stream thing))))

(defun process-request (request reply type thunk)
  (let+ ((stream)
         ((&flet ensure-stream ()
            (or stream
                (let ((raw-stream (hunchentoot::start-output
                                   (hunchentoot::return-code reply))))
                  (setf stream (flexi-streams:make-flexi-stream
                                raw-stream :external-format :utf-8))))))
         ((&flet error-reply (code condition)
            (setf (hunchentoot:return-code*) code)
            (api-reply (ensure-stream) type condition))))
    (handler-case
        (let ((data-or-thunk (funcall thunk request)))
          (api-reply (ensure-stream) type data-or-thunk))
      ((or missing-required-argument incompatible-arguments
           argument-error)
          (condition)
        (error-reply hunchentoot:+http-bad-request+ condition))
      (error (condition)
        (error-reply hunchentoot:+http-internal-server-error+ condition)))))

(defun make-api-endpoint-documentation (thunk stream)
  (let+ (((&flet %split-paragraphs (text)
            (reduce (lambda+ ((previous paragraph paragraphs) current)
                      (cond
                        ((not (and (eql previous #\Newline)
                                   (eql current #\Newline)))
                         (let ((paragraph (or paragraph
                                              (make-string-output-stream))))
                           (write-char current paragraph)
                           (list current paragraph paragraphs)))
                        (paragraph
                         (let ((text (get-output-stream-string paragraph)))
                           (list current nil (list* text paragraphs))))))
                    text :initial-value '(nil nil ()))))
         ((&flet split-paragraphs (text)
            (let+ (((&ign paragraph paragraphs) (%split-paragraphs text)))
              (nreverse (if paragraph
                            (list* (get-output-stream-string paragraph) paragraphs)
                            paragraphs)))))
         ((&values documentation bindings) (funcall thunk :documentation)))
    (format stream "<!DOCTYPE html>~@
                    <html xmlns=\"http://www.w3.org/1999/xhtml\">~@
                      <head>~@
                        <meta charset=\"utf-8\"/>~@
                        <title>API Endpoint</title>~@
                        <link rel=\"stylesheet\" type=\"text/css\" href=\"/base.css\"/>
                      </head>~@
                      <body>~@
                        ~{<p>~A</p>~}~@
                        ~@[~
                          <p>~@
                            Accepted query parameters:~@
                            <ul>~{~
                              <li>~{~
                                <code>~
                                  ~A~@[: ~(~A~)~]~:[~*~;= ~A~] ~
                                  [~:[required~;optional~]]~
                                </code>~@
                                ~{<p>~A</p>~}~
                              ~}</li>~@
                            ~} </ul>~@
                          </p>~
                        ~]~@
                      </body>~@
                    </html>"
            (split-paragraphs documentation)
            (map-parsed-arguments-specs
             (lambda (var name type transform default default? optional? documentation)
               (declare (ignore var transform))
               (list name type default? default optional?
                     (split-paragraphs documentation)))
             bindings))))

(defun call-providing-api-endpoint (request reply thunk)
  (let* ((accept    (hunchentoot:header-in "Accept" request))
         (preferred (first (split-sequence:split-sequence #\, accept))))
    (switch (preferred :test #'string=)
      ("application/json"
       (setf (hunchentoot:header-out "Content-type" reply)
             "application/json;charset=UTF-8")
       (process-request request reply :json thunk))
      ("text/html"
       (setf (hunchentoot:header-out "Content-type") "text/html")
       (with-output-to-string (stream)
         (make-api-endpoint-documentation thunk stream)))
      (t
       (setf (hunchentoot:return-code*)
             hunchentoot:+http-unsupported-media-type+)
       (hunchentoot:abort-request-handler)))))

(defmacro providing-api-endpoint ((&key
                                   (request 'hunchentoot:*request*)
                                   (reply   'hunchentoot:*reply*))
                                  bindings &body body)
  "Arrange for BODY to process REQUEST with parameters according to
   BINDINGS and producing its reply using REPLY.

   BINDINGS contains parameter declarations using the syntax expected
   by `with-arguments'."
  (let+ (((&values body &ign documentation)
          (parse-body body :documentation t)))
    `(call-providing-api-endpoint
      ,request ,reply (lambda (,request)
                        (if (eq ,request :documentation)
                            (values ,documentation ',bindings)
                            (with-arguments (,request ,@bindings)
                              ,@body))))))

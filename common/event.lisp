;;;; event.lisp --- Event construction utilities.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.common)

(defun parse-payload-spec (spec)
  "Parse SPEC as an empty payload, a reference to standard input, a
   pathname or a Lisp object."
  (flet ((parse-file-based-payload-spec (spec &key (allow-binary? t))
           (cond
             ;; "-[:binary]" => read from standard input.
             ((string= spec "-")
              (read-stream-content-into-string *standard-input*))
             ((string= spec "-:binary")
              (read-stream-content-into-byte-vector *standard-input*))

             ;; "#p"NAMESTRING"[:(binary|EXTERNAL-FORMAT)]" => read
             ;; from file.
             ((ppcre:register-groups-bind (namestring)
                  ("^#[pP]\"([^\"]+)\":binary$" spec)
                (unless allow-binary?
                  (error "~@<Binary content of file ~S cannot be used in ~
                            this context.~@:>"
                         namestring))
                (read-file-into-byte-vector (parse-namestring namestring))))
             ((ppcre:register-groups-bind (namestring external-format)
                  ("^#[pP]\"([^\"]+)\"(?::(.+))?$" spec)
                (let ((pathname        (parse-namestring namestring))
                      (external-format (when external-format
                                         (make-keyword
                                          (string-upcase external-format)))))
                  (apply #'read-file-into-string pathname
                         (when external-format
                           (list :external-format external-format)))))))))
    (cond
      ;; Empty string => no value.
      ((emptyp spec)
       rsb.converter:+no-value+)

      ;; "true", "false" => t, nil
      ((string= spec "true")
       t)
      ((string= spec "false")
       nil)

      ;; "/SCOPE" => parse scope.
      ((starts-with #\/ spec)
       (make-scope spec))

      ;; File-based specs.
      ((parse-file-based-payload-spec spec))

      ;; "pb:MESSAGE-NAME:(PATHNAME-SPEC|STDIN-SPEC|PROTOBUF-DEBUG-FORMAT)"
      ;; => parse protocol buffer debug text format and construct
      ;; message of type MESSAGE-NAME.
      ((ppcre:register-groups-bind (descriptor body)
           ("^pb:([^:]+):((?:.|\\n)*)$" spec)
         (let ((fields (or (when-let ((string (parse-file-based-payload-spec
                                               body :allow-binary? nil)))
                             (string-trim '(#\Space #\Tab #\Newline) string))
                           body)))
           (build-protocol-buffer-message descriptor fields))))

      ;; Otherwise try to `cl:read', potentially signaling an error.
      (t
       (let+ (((&values value consumed) (read-from-string spec)))
         (unless (= consumed (length spec))
           (error "~@<Junk at end of argument string: ~S.~@:>"
                  (subseq spec consumed)))
         value)))))

(defun parse-call-spec (spec)
  (with-condition-translation (((error call-specification-error)
                                :specification spec))
    (ppcre:register-groups-bind
        (server-uri method arg)
        ("^(?:([-_a-zA-Z0-9/:&?#=+;]+))?/([-_a-zA-Z0-9]+)\\((.*)\\)$" spec)
      (return-from parse-call-spec
        (values (rsb::parse-scope-or-uri (or server-uri "/"))
                method
                (parse-payload-spec arg))))
    (error "~@<The specification is not of the form ~
            SERVER-URI/METHOD([ARGUMENT])~@:>"
           spec)))

(defun parse-pair (pair
                   &key
                   (separator        #\=)
                   (first-transform  #'identity)
                   (second-transform #'identity))
  (let ((index (or (position separator pair)
                   (error "~@<~S is not of the form KEY~CVALUE.~@:>"
                          pair separator))))
    (list (funcall first-transform (subseq pair 0 index))
          (funcall second-transform (subseq pair (1+ index))))))

(defun parse-meta-data (value)
  (parse-pair value :first-transform #'make-keyword))

(defun parse-timestamp (value)
  (parse-pair value :first-transform  #'make-keyword
              :second-transform #'local-time:parse-timestring))

(defun parse-cause (value)
  (apply #'cons
         (parse-pair value :separator        #\:
                     :first-transform  #'uuid:make-uuid-from-string
                     :second-transform #'parse-integer)))

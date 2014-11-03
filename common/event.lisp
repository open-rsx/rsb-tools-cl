;;;; event.lisp --- Event construction utilities.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.common)

(defun parse-payload-spec (spec)
  "Parse SPEC as an empty payload, a reference to standard input, a
   pathname or a Lisp object."
  (cond
    ((emptyp spec)
     rsb.converter:+no-value+)

    ((string= spec "true")
     t)
    ((string= spec "false")
     nil)

    ((string= spec "-")
     (read-stream-content-into-string *standard-input*))

    ((string= spec "-:binary")
     (read-stream-content-into-byte-vector *standard-input*))

    ((ppcre:register-groups-bind (namestring)
         ("^#[pP]\"([^\"]+)\":binary$" spec)
       (read-file-into-byte-vector (parse-namestring namestring))))

    ((ppcre:register-groups-bind (namestring external-format)
         ("^#[pP]\"([^\"]+)\"(?::(.+))?$" spec)
       (let ((pathname        (parse-namestring namestring))
             (external-format (when external-format
                                (make-keyword (string-upcase external-format)))))
         (apply #'read-file-into-string pathname
                (when external-format
                  (list :external-format external-format))))))

    (t
     (let+ (((&values value consumed) (read-from-string spec)))
       (unless (= consumed (length spec))
         (error "~@<Junk at end of argument string: ~S.~@:>"
                (subseq spec consumed)))
       value))))

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

;;;; util.lisp --- Utility functions for event formatting.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.formatting)

;;; Extractor functions

;; TODO duplicated in stats/util.lisp. Maybe we can fix that at some
;; point.
(defun event-size (event &optional (replacement-value :n/a))
  "Try to determine and return the size of the payload of EVENT in
   bytes. Return REPLACEMENT-VALUE, if the size cannot be determined."
  (labels ((payload-size (payload)
             (typecase payload
               (integer
                (ceiling (integer-length payload) 8))
               (rsb.converter::annotated
                (payload-size (rsb.converter::annotated-wire-data payload)))
               ((cons t (not list))
                replacement-value)
               (sequence
                (length payload))
               (t
                replacement-value))))
    (or (meta-data event :rsb.transport.payload-size)
        (payload-size (event-data event)))))

;;; Timestamp handling

(declaim (ftype (function (local-time:timestamp) timestamp/unix/nsec)
                timestamp->unix/nsecs)
         (inline timestamp->unix/nsecs))

(defun timestamp->unix/nsecs (timestamp)
  "Return the number of nanoseconds since UNIX epoch for TIMESTAMP."
  (let ((secs  (local-time:timestamp-to-unix timestamp))
        (nsecs (local-time:nsec-of timestamp)))
    (declare (type non-negative-integer  secs)
             (type (integer 0 999999999) nsecs))
    (+ (* secs 1000000000) nsecs)))

(defvar *keyword-readtable*
  (let ((readtable (with-standard-io-syntax (copy-readtable))))
    (setf (readtable-case readtable) :invert)
    readtable))

(defun timestamp-name (name)
  "For non-framework timestamps return \"*NAME\"."
  (if (framework-timestamp? name)
      (string-downcase name)
      (with-standard-io-syntax
        (let ((*readtable* *keyword-readtable*))
          (format nil "*~A" name)))))

;;; Predicates

(defun request-event? (event)
  "Return non-nil when EVENT is a request."
  (eq (event-method event) :|request|))

(defun reply-event? (event)
  "Return non-nil when EVENT is a reply."
  (eq (event-method event) :|reply|))

(defun error-event? (event)
  "Return non-nil if EVENT is a reply indicating an error."
  (meta-data event :|rsb:error?|))

(defun framework-timestamp? (name)
  "Return non-nil if NAME names a framework timestamp."
  (member name *framework-timestamps*))

;;; Formatting functions

(defun format-method (stream event &optional colon? at?)
  "Format called method of EVENT onto STREAM."
  (declare (ignore colon? at?))
  (let ((components (scope-components (event-scope event))))
    (format stream "~{/~A~}::~A" ; TODO format-scope-components
            (butlast components) (lastcar components))))

(defun print-timestamp (stream timestamp &optional colon? at?)
  (declare (ignore at?))
  (cond
    ((not timestamp)
     (format stream "N/A"))
    ((not colon?)
     (local-time:format-timestring stream timestamp))
    (t
     (local-time:format-timestring
      stream timestamp
      :format '((:hour 2) #\: (:min 2) #\: (:sec 2) #\. (:usec 6))))))

;;;

(defun format-maybe (stream value)
  "Print VALUE onto STREAM unless it is nil."
  (format stream "~:[N/A~;~:*~A~]" value))

(defun format-aligned-items (stream keys values
                             &key
                             (value-formatter #'format-maybe))
  "Format KEYS and VALUES onto STREAM such that keys and values align
   vertically across output lines."
  (let ((width (reduce #'max keys
                       :key           (compose #'length #'string)
                       :initial-value 0)))
    (iter (for key   in keys)
          (for value in values)
          (unless (first-iteration-p)
            (format stream "~&"))
          (format stream "~:(~VA~): " width key)
          (funcall value-formatter stream value))))

(declaim (special *tracker*))

(defvar *tracker* nil
  "This variable should be dynamically bound to a hash-table which is
   then used by `format-recursively' to detect already formatted
   objects.")

(defun format-recursively (stream value
                           &key
                           (tracker (or *tracker* (make-hash-table))))
  "TODO(jmoringe): document"
  (let ((*tracker* tracker))
    (if (gethash value tracker)
        (format stream "~A" value)
        (progn
          (setf (gethash value tracker) t)
          (etypecase value
            (string
             (format stream "~S" value))
            ((and (array (unsigned-byte 8) (*))
                  (not (array (unsigned-byte 8) (0))))
             (pprint-logical-block (stream (list value))
               (format-payload value :any stream)))
            (sequence
             (if (emptyp value)
                 (format stream "<empty sequence>")
                 (progn
                   (pprint-newline :mandatory stream)
                   (pprint-logical-block (stream (list value)
                                                 :per-line-prefix "  ")
                     (iter (for item in-sequence value)
                           (unless (first-iteration-p)
                             (format stream "~@:_"))
                           (format-recursively stream item))))))
            (standard-object
             (format-instance stream value))
            (t
             (format stream "~A" value)))))))

(defun format-instance (stream instance)
  "Format INSTANCE onto STREAM, handling slot values recursively."
  (let+ (((&flet slot-value* (name)
            (if (slot-boundp instance name)
                (slot-value instance name)
                "UNBOUND")))
         (keys   (map 'list #'closer-mop:slot-definition-name
                      (closer-mop:class-slots (class-of instance))))
         (values (map 'list #'slot-value* keys)))
    (format stream "~A~@:_" (class-name (class-of instance)))
    (pprint-logical-block (stream (list instance)
                                  :per-line-prefix "  ")
      (format-aligned-items stream keys values
                            :value-formatter #'format-recursively))))

;;; Stream-related functions

(defun stream-line-width (stream)
  "Return the line width of STREAM or nil, if it cannot be
   determined."
  (when-let* ((package (find-package :com.dvlsoft.clon))
              (symbol  (find-symbol "STREAM-LINE-WIDTH" package)))
    (ignore-errors (funcall symbol stream))))

(defmacro with-print-limits ((stream) &body body)
  "Execute BODY with `*print-right-margin*' and `*print-miser-width*'
   bound to the line width of STREAM. Additionally, install a handler
   for SIGWINCH that updates these values, if possible."
  `(call-with-print-limits ,stream (lambda () ,@body)))

(defun call-with-print-limits (stream thunk)
  "Call THUNK with `*print-right-margin*' and ``*print-miser-width*'
   bound to the suitable values for the line width of
   STREAM. Additionally, install a handler for SIGWINCH that updates
   these values, if possible."
  (let* ((thread               (bt:current-thread))
         (*print-right-margin* (stream-line-width stream))
         (*print-miser-width*  nil)
         (*print-length*       64))
    #+(and sbcl (not win32))
    (sb-unix::enable-interrupt
     sb-unix:SIGWINCH
     (lambda (signal info context)
       (declare (ignore signal info context))
       (bt:interrupt-thread
        thread
        (lambda () (setf *print-right-margin* (stream-line-width stream))))))
    (funcall thunk)))

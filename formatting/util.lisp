;;;; util.lisp --- Utility functions for event formatting.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsb.formatting)

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

(defun timestamp-name (name)
  "For non-framework timestamps return \"*NAME\"."
  (if (framework-timestamp? name)
      name
      (concatenate 'string "*" (string name))))

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
    (format stream "~{/~A~}::~A"
            (subseq components 0 (- (length components) 2))
            (lastcar components))))

;;;

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defmacro with-indent ((stream-var
                          &key
                          (amount              2)
                          (initial-fresh-line? t)
                          (final-fresh-line?   t))
                         &body body)
    "Execute BODY with the stream of the current emit target bound to a
pretty-printing stream that indents all output produced within BODY to
a certain depth. In addition, a scope of kind KIND and name NAME is
printed around the output."
    `(let ((previous *print-right-margin*))
       (unwind-protect
            (progn
              (when *print-right-margin*
                (decf *print-right-margin* ,amount))
              ,@(when initial-fresh-line?
                  `((format ,stream-var "~&")))
              (pprint-logical-block (,stream-var nil
                                                 :per-line-prefix ,(make-string amount :initial-element #\Space))
                ,@body)
              ,@(when final-fresh-line?
                  `((format ,stream-var "~&"))))
         (setf *print-right-margin* previous))))

  (defmacro with-indented-section ((stream-var title
                                    &key
                                    (amount            2)
                                    (final-fresh-line? t))
                                   &body body)
    "Execute BODY with STREAM-VAR bound to a stream that indents all
content by AMOUNT."
    `(progn
       ,@(when title
               `((format ,stream-var (format nil "~A~~&" ,title))))
       (with-indent (,stream-var :amount              ,amount
                                 :initial-fresh-line? nil
                                 :final-fresh-line?   ,final-fresh-line?)
         ,@body))))

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

(defun format-aligned-items/alist (stream items
                                   &key
                                   value-formatter)
  "Format ITEMS in which each item is of the form (KEY . VALUE) onto
STREAM such that keys and values align vertically across output
lines."
  (format-aligned-items
   stream (map 'list #'car items) (map 'list #'cdr items))
  :value-formatter value-formatter)

(defun format-pairs/plist (stream &rest items)
  "Format keys and values of the plist ITEMS onto STREAM such that
keys and values align vertically across output lines."
  (format-aligned-items
   stream
   (iter (for item in items        :by #'cddr) (collect item))
   (iter (for item in (rest items) :by #'cddr) (collect item))))

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
             (format-payload value :any stream))
            (sequence
             (if (emptyp value)
                 (format stream "<empty sequence>")
                 (with-indent (stream :final-fresh-line? nil)
                   (iter (for item in-sequence value)
                         (unless (first-iteration-p)
                           (format stream "~&"))
                         (format-recursively stream item)))))
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
    (format stream "~A" instance)
    (with-indent (stream :final-fresh-line? nil)
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
bound to the line width of STREAM. Additionally, install a handler for
SIGWINCH that updates these values, if possible."
  `(invoke-with-print-limits ,stream (lambda () ,@body)))

(defun invoke-with-print-limits (stream thunk)
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

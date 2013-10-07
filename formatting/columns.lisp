;;;; columns.lisp --- Some column classes.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsb.formatting)

;;; `basic-column'

(defclass basic-column (name-mixin)
  ()
  (:documentation
   "Superclass for column classes."))

(defmethod format-header ((column basic-column)
                          (stream t))
  (format stream "~@(~A~)" (column-name column)))

;;; Simple columns

(macrolet ((define-simple-column ((name width
                                   &key
                                   (event-class 'event)
                                   (print-name  (string name)))
                                  &body doc-and-body)
             (let+ (((&optional width (alignment :right)) (ensure-list width))
                    (class-name  (symbolicate "COLUMN-" name))
                    ((&values body nil doc)
                     (parse-body doc-and-body :documentation t)))
               `(progn
                  (defmethod find-column-class ((spec (eql ,name)))
                    (find-class ',class-name))

                  (defclass ,class-name (,@(when width
                                           '(width-mixin))
                                         basic-column)
                    ()
                    ,@(when width
                        `((:default-initargs
                           :width     ,width
                           :alignment ,alignment
                           :name      ,print-name)))
                    ,@(when doc
                        `((:documentation ,doc))))

                  ,@(unless width
                      `((defmethod column-width ((column ,class-name))
                          0)))

                  (defmethod format-event ((event  ,event-class)
                                           (column ,class-name)
                                           (stream t)
                                           &key &allow-other-keys)
                    ,@body)))))

  ;; Output control
  (define-simple-column (:same-line nil
                         :event-class t)
      "Put carriage at beginning of the current line, causing the
current content to be overridden by subsequent output."
    (format stream "~C" #\Return))
  (define-simple-column (:newline nil
                         :event-class t)
      "Start a new line of output."
    (terpri stream))
  (define-simple-column (:flush nil
                         :event-class t)
      "Flush buffers of the output stream."
    (force-output stream))

  ;; Event-independent
  (define-simple-column (:now 32
                         :event-class t)
      "Emit the current time."
    (format stream "~A" (local-time:now)))
  (define-simple-column (:now/compact 15
                         :print-name  "now"
                         :event-class t)
      "Emit the current time in a compact format."
    (local-time:format-timestring
     stream (local-time:now)
     :format '((:hour 2) #\: (:min 2) #\: (:sec 2) #\. (:usec 6))))
  (define-simple-column (:text 32
                         :event-class t)
      "Emit a given text. The name of the column is also the emitted
text."
    (format stream "~A" (column-name column)))

  ;; Event properties
  (define-simple-column (:origin (8 :left))
      "Emit an abbreviated representation of the id of the participant
at which the event originated."
    (format stream "~:[ORIGIN? ~;~:*~/rsb::print-id/~]"
            (event-origin event)))

  (define-simple-column (:origin-full (36 :left))
      "Emit the id of the participant at which the event originated."
    (format stream "~:[ORIGIN? ~;~:*~:/rsb::print-id/~]"
            (event-origin event)))

  (define-simple-column (:sequence-number 8
                         :print-name "Sequence Number")
      "Emit the sequence number of the event."
    (format stream "~8,'0X" (event-sequence-number event)))

  (define-simple-column (:method (10 :left))
      "Emit the method of the event. If the event does not have an id,
the string \"<nomethod>\" is emitted instead."
    (format stream "~:[<nomethod>~;~:*~:@(~10A~)~]"
            (event-method event)))

  (define-simple-column (:id (8 :left))
      "Emit an abbreviated representation of the id of the event."
    (format stream "~:[EVENTID? ~;~:*~/rsb::print-id/~]"
            (event-id event)))

  (define-simple-column (:id-full (36 :left))
      "Emit the id of the event."
    (format stream "~:[EVENTID? ~;~:*~:/rsb::print-id/~]"
            (event-id event)))

  (define-simple-column (:scope (24 :left))
      "Emit the scope of the event."
    (format stream "~A" (scope-string (event-scope event))))

  (define-simple-column (:wire-schema (24 :left))
      "Emit wire-schema of the event, if possible."
    (format stream "~:[WIRE-SCHEMA?~;~:*~A~]"
            (meta-data event :rsb.transport.wire-schema)))

  (define-simple-column (:data (21 :left))
      "Emit a representation of the data contained in the event."
    (let ((*print-length* (column-width column)))
      (format stream "~/rsb::print-event-data/" (event-data event))))

  (define-simple-column (:data-size 9
                         :print-name "Data Size")
      "Emit an indication of the size of the data contained in the
event, if the size can be determined."
    (let* ((data (event-data event))
           (size (typecase data
                   (sequence (length data)))))
      (format stream "~:[N/A~;~:*~,,,3:D~]" size)))

  (define-simple-column (:notification-size 9
                         :print-name "Notification Size")
      "Emit an indication of the size of the notification in which the
event has been transmitted, if the size can be determined."
    (format stream "~:[N/A~;~:*~,,,3:D~]"
            (meta-data event :rsb.transport.notification-size)))

  ;; Request/Reply stuff
  (define-simple-column (:call (57 :left))
      "Emit a method call description. Should only be applied to
events that actually are method calls."
    (let ((*print-length* most-positive-fixnum))
      (format stream "~/rsb.formatting::format-method/(~/rsb::print-event-data/)"
              event (event-data event))))

  (define-simple-column (:call-id (8 :left))
      "Emit the request id of a reply event. Should only be applied to
events that actually are replies to method calls."
      (format stream "~:[CALLID? ~;~:*~/rsb::print-id/~]"
              (when-let ((cause (first (event-causes event))))
                (event-id->uuid cause))))

  (define-simple-column (:result (57 :left))
      "Emit a method reply description. Should only be applied to
events that actually are replies to method calls."
    (let ((*print-length* most-positive-fixnum))
      (format stream "> ~/rsb.formatting::format-method/ ~
~:[=>~;ERROR:~] ~/rsb::print-event-data/"
              event (error-event? event) (event-data event)))))

;;; Constant column

(defmethod find-column-class ((spec (eql :constant)))
  (find-class 'column-constant))

(defclass column-constant (width-mixin
                           basic-column)
  ((value     :initarg  :value
              :accessor column-value
              :documentation
              "Stores the constant value emitted by the column.")
   (formatter :initarg  :formatter
              :type     function
              :accessor column-formatter
              :initform #'princ
              :documentation
              "Stores a function that is called to print the value of
the column onto a destination stream."))
  (:default-initargs
   :value (missing-required-initarg 'column-constant :value))
  (:documentation
   "Instances of this column class emit a print a specified constant
value."))

(defmethod format-event ((event  t)
                         (column column-constant)
                         (stream t)
                         &key &allow-other-keys)
  (funcall (column-formatter column) (column-value column) stream))

;;; Timestamp and meta-data columns

(macrolet
    ((define-meta-data-column ((name
                                &key
                                (accessor (intern (string name) :rsb))))
       (let ((class-name (intern (string name))))
         `(progn
            (defmethod find-column-class ((spec (eql ,name)))
              (find-class ',class-name))

            (defclass ,class-name (width-mixin
                                   basic-column)
              ((key :initarg  :key
                    :type     symbol
                    :reader   column-key
                    :documentation
                    "Stores the key of the meta-data item that should
be extracted from events."))
              (:default-initargs
               :key       (missing-required-initarg ',class-name :key)
               :width     32
               :alignment :left)
              (:documentation
               ,(format nil "Emit the ~(~A~) of the event designated by ~
the value of the :key initarg."
                        name)))

            (defmethod shared-initialize :after ((instance   ,class-name)
                                                 (slot-names t)
                                                 &key)
                       (setf (column-name instance)
                             (format nil "~@(~A~)" (column-key instance))))

            (defmethod format-event ((event  event)
                                     (column ,class-name)
                                     (stream t)
                                     &key &allow-other-keys)
              (format stream "~:[N/A~;~:*~A~]"
                      (,accessor event (column-key column))))))))

  (define-meta-data-column (:timestamp))
  (define-meta-data-column (:meta-data)))

;;; Count column

(defmethod find-column-class ((spec (eql :count)))
  (find-class 'column-count))

(defclass column-count (width-mixin
                        basic-column)
  ((count :initarg  :count
          :type     non-negative-integer
          :accessor column-count
          :initform 0
          :documentation
          "Stores the number of performed output operations."))
  (:default-initargs
   :width 8
   :name  "Count")
  (:documentation
   "Count the number of output operations and emit that number."))

(defmethod format-event ((event  t)
                         (column column-count)
                         (stream t)
                         &key &allow-other-keys)
  (format stream "~:D" (incf (column-count column))))

;;; Some useful column and style specifications
;;;
;;; For use in `columns-mixin' and subclasses such as
;;; `style-compact/*' `style-statistics/*' and `style-monitor/*'.

(defvar *basic-columns*
  '(;; Quantities
    (:rate/12       . '(:quantity :quantity :rate       :width 12))
    (:throughput/13 . '(:quantity :quantity :throughput :width 13))
    (:latency       . '(:quantity :quantity (:latency
                                             :from :create
                                             :to   :deliver
                                             :name "Latency")))
    (:origin/40     . '(:quantity :quantity :origin     :width 40 :alignment :left))
    (:scope/40      . '(:quantity :quantity :scope      :width 40 :alignment :left))
    (:type/40       . '(:quantity :quantity :type       :width 40 :alignment :left))
    (:size/20       . '(:quantity :quantity :size       :width 20)))
  "Contains an alist of column specification entries of the form

  (NAME . SPEC)

where NAME names the column specification SPEC. See `columns-mixin'
for information regarding the processing of SPEC.")

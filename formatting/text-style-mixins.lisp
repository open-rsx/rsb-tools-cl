;;;; text-style-mixins.lisp --- Mixin classes for textual formatting style classes.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.formatting)

;;; `periodic-printing-mixin'

(defclass periodic-printing-mixin ()
  ((print-interval :initarg  :print-interval
                   :type     print-interval
                   :accessor style-print-interval
                   :initform 1
                   :documentation
                   "Stores the amount of time in seconds between
                    successive print operations.")
   (stream         :type     (or null stream)
                   :accessor style-%stream
                   :initform nil
                   :documentation
                   "Stores the stream that should be used for periodic
                    printing.")
   (pretty-state   :type     list
                   :accessor style-%pretty-state
                   :documentation
                   "Stores the pretty-printer state that should be
                    used for periodic printing.")
   (timer          :accessor style-%timer
                   :documentation
                   "Stores the timer used to trigger periodic
                    printing.")
   (lock           :reader   style-%lock
                   :initform (bt:make-recursive-lock
                              "Periodic printing lock")
                   :documentation
                   "Stores a lock that protects timer-triggered
                    accesses to the style object against
                    `format-event'-triggered accesses."))
  (:documentation
   "This mixin class is intended to be mixed into formatting classes
    that produce output periodically instead of being triggered by the
    arrival of events.

    When `format-event' is called, an :around method prevents
    output-producing methods from running. Instead, these method are
    run a timer-driven way."))

(defmethod initialize-instance :after ((instance periodic-printing-mixin)
                                       &key)
  (let+ (((&structure style- (timer %timer) print-interval) instance)
         (timer* #+sbcl (sb-ext:make-timer (%make-timer-function instance)
                                           :thread t)
                 #-sbcl #.(error "not implemented")))
    ;; Store and activate the timer.
    (setf timer          timer*
          print-interval print-interval) ; trigger scheduling

    ;; Register a finalizer to stop the timer. We need the timer*
    ;; variable since the finalizer closure must not perform slot
    ;; accesses on INSTANCE.
    (tg:finalize instance (lambda () (sb-ext:unschedule-timer timer*)))))

(defmethod (setf style-print-interval) :before ((new-value t)
                                                (style     periodic-printing-mixin))
  "Validate NEW-VALUE to prevent bad timer scheduling."
  (check-type new-value print-interval))

(defmethod (setf style-print-interval) :after ((new-value t)
                                               (style     periodic-printing-mixin))
  "After storing the new print-interval value, reschedule the timer."
  (let+ (((&accessors-r/o (timer style-%timer)) style))
    #-sbcl #.(error "not implemented")
    #+sbcl (sb-ext:unschedule-timer timer)
    (when new-value
      #+sbcl (sb-ext:schedule-timer timer new-value
                                    :repeat-interval new-value))))

(defmethod collects? ((style periodic-printing-mixin))
  t)

(defmethod format-event :around ((event  t)
                                 (style  periodic-printing-mixin)
                                 (stream t)
                                 &key &allow-other-keys)
  "Protect against concurrent access to STYLE and store STREAM for use
   in timer-driven output."
  (bt:with-recursive-lock-held ((style-%lock style))
    (unless (eq event :trigger)
      (setf (style-%stream style)       stream
            (style-%pretty-state style) (list *print-right-margin*
                                              *print-miser-width*)))

    (call-next-method)))

;; Utility functions

(defun %make-timer-function (style)
  "Return a function that is weakly-closed over STYLE and tries to run
   STYLE's `format-event' function when called."
  (let ((weak-style (tg:make-weak-pointer style)))
    (lambda ()
      (when-let ((style  (tg:weak-pointer-value weak-style))
                 (stream (style-%stream style)))
        (let+ (((*print-right-margin* *print-miser-width*)
                (style-%pretty-state style)))
          (ignore-some-conditions (stream-error)
            (format-event :trigger style stream)))))))

;;; `output-buffering-mixin'

(defclass output-buffering-mixin ()
  ()
  (:documentation
   "This mixin class provides buffering of output. This can be useful
    when lots of output has to be produced and written. This can, for
    example, reduce flickering."))

(defmethod format-event :around ((event  (eql :trigger))
                                 (style  output-buffering-mixin)
                                 (stream t)
                                 &rest args &key &allow-other-keys)
  (write-string
   (with-output-to-string (stream)
     (apply #'call-next-method event style stream args))
   stream))

;;; `header-printing-mixin'

(defclass header-printing-mixin (counting-mixin)
  ((header-frequency :initarg  :header-frequency
                     :type     (or null positive-integer)
                     :accessor style-header-frequency
                     :initform 22
                     :documentation
                     "Stores the number of output cycles after which a
                      header should be printed or nil in case a header
                      is never printed."))
  (:documentation
   "This class is intended to be mixed into formatting style classes
    that periodically print a header of some kind into their regular
    stream of output."))

(defmethod format-event :before ((event  t)
                                 (style  header-printing-mixin)
                                 (stream t)
                                 &key &allow-other-keys)
  (let+ (((&structure-r/o style- header-frequency) style))
    (when (and header-frequency
               (zerop (mod (style-count style) header-frequency)))
      (format-header style stream))))

;;; `separator-mixin'

(defclass separator-mixin ()
  ((separator :type     separator-spec
              :accessor style-separator
              :initform #\Newline
              :documentation
              "The character or pattern by means of which items should
               be separated in the output."))
  (:documentation
   "This class is intended to be mixed into style classes that should
    print separators between output items."))

(defmethod shared-initialize :after ((instance   separator-mixin)
                                     (slot-names t)
                                     &key
                                     (separator nil separator-supplied?))
  (when separator-supplied?
    (setf (style-separator instance) separator)))

(defmethod (setf style-separator) :before ((new-value t)
                                           (style     separator-mixin))
  (check-type new-value separator-spec "a valid separator specification"))

(defmethod format-event :before ((event  t)
                                 (style  separator-mixin)
                                 (stream t)
                                 &key
                                 (max-columns (or *print-right-margin* 80))
                                 &allow-other-keys)
  "Print a separator before each event."
  (print-separator (style-separator style) stream max-columns))

;; Utility functions

(defun print-separator (spec stream max-columns)
  "Print a separator according to SPEC onto STREAM."
  (etypecase spec
    (null)

    ((or character string)
     (princ spec stream))

    (rule-spec
     (princ (make-string max-columns :initial-element (second spec))
            stream))

    ((eql :clear)
     (format stream "~C~A~C~A" #\Escape "[1;1f" #\Escape "[J"))

    (list
     (map nil (rcurry #'print-separator stream max-columns) spec))))

;;; `width-mixin'

(defclass width-mixin ()
  ((width     :initarg  :width
              :type     positive-integer
              :accessor column-width
              :initform 16
              :documentation
              "Stores the maximum acceptable output width for the
               formatter instance.")
   (alignment :initarg  :alignment
              :type     (member :left :right)
              :accessor column-alignment
              :initform :right
              :documentation
              "Stores the alignment that should be employed by the
               formatter instance."))
  (:documentation
   "This class is intended to be mixed into formatting classes that
    should produce output of a fixed width."))

(defmethod format-header :around ((column width-mixin)
                                  (stream t))
  (call-with-width-limit
   stream (column-width column) :left
   (lambda (stream) (call-next-method column stream))))

(defmethod format-event :around ((event  t)
                                 (style  width-mixin)
                                 (stream t)
                                 &key &allow-other-keys)
  (call-with-width-limit
   stream (column-width style) (column-alignment style)
   (lambda (stream) (call-next-method event style stream))))

(defmethod print-object ((object width-mixin) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~D ~A"
            (column-width object) (column-alignment object))))

;; Utility functions

(defun call-with-width-limit (stream limit align thunk)
  "Call THUNK with a single argument that is a stream. Format things
   printed to the stream by THUNK on STREAM ensuring a width limit
   LIMIT and alignment according to ALIGN. ALIGN can be :left
   or :right."
  (let* ((value  (with-output-to-string (stream)
                   (funcall thunk stream)))
         (length (length value)))
    (cond
      ;; No room at all - print nothing.
      ((zerop limit))

      ;; Only room for a single character - print ellipsis if we have
      ;; any output.
      ((< limit 1 length)
       (format stream "…"))

      ;; Not enough room - print value and ellipsis.
      ((< limit length)
       (ecase align
         (:left
          (format stream "~A…" (subseq value 0 (- limit 1))))
         (:right
          (format stream "…~A" (subseq value (1+ (- length limit)))))))

      ;; Enough room - pad value.
      (t
       (ecase align
         (:left
          (format stream "~VA" limit value))
         (:right
          (format stream "~V@A" limit value)))))))

(defmacro with-width-limit ((stream-var limit align) &body body)
  "Execute BODY with a STREAM-VAR bound to a stream. Format things
   sprinted to the value of STREAM-VAR in BODY on the previous value of
   STREAM-VAR ensuring a width limit LIMIT and alignment according to
   ALIGN. ALIGN can be :left or :right."
  `(call-with-width-limit ,stream-var ,limit ,align
                          (lambda (,stream-var) ,@body)))

;;; `columns-mixin'

(eval-when (:compile-toplevel)
  (defmacro when-column-fits ((column separator
                               position
                               produced-output? printed-ellipsis?-var)
                              &body body)
    "Execute BODY if the state captured by POSITION, PRODUCED-OUTPUT?
     and PRINTED-ELLIPSIS?-VAR permits printing COLUMN and optionally
     SEPARATOR."
    (once-only (position column produced-output? separator)
      `(if (columns-exhausted? (+ ,position
                                  (if ,produced-output? (length ,separator) 0)
                                  (column-width ,column)))
           (unless ,printed-ellipsis?-var
             (format stream ">")
             (setf ,printed-ellipsis?-var t))
           (progn ,@body)))))

(defclass columns-mixin ()
  ((columns   :type     list
              :accessor style-columns
              :accessor style-%columns ; does not process column specs
              :initform '()
              :documentation
              "Stores the list of columns of which the formatting
               style is composed.")
   (separator :initarg  :separator
              :type     string
              :accessor style-separator
              :initform (if *textual-output-can-use-utf-8?* "│" "|")
              :documentation
              "Stores a separator string that is printed between the
               output produced by adjacent columns."))
  (:documentation
   "This mixin class is intended to be mixed into formatting styles
    that produce column-based output. When combined with
    `header-printing-mixin', column names are used to produce header
    lines.

    When setting columns via the :columns initarg or \(setf
    style-columns\), a list of either column instances or column
    specifications can be used. A column specification is either a
    keyword designating a class in the column class family or a list
    of the form

      (CLASS KEY1 VALUE1 KEY2 VALUE2 ...)

    consisting of the keyword CLASS and initargs for the column class
    designated by CLASS."))

(defmethod shared-initialize :after ((instance   columns-mixin)
                                     (slot-names t)
                                     &key
                                     (columns nil columns-supplied?))
  ;; Interpret column specs in COLUMNS, if it has been supplied.
  (when columns-supplied?
    (setf (style-columns instance) columns)))

(defmethod (setf style-columns) :around ((new-value list)
                                         (style     columns-mixin))
  ;; Interpret each element of NEW-VALUE as a column instance or a
  ;; specification for creating a column instance.
  (call-next-method (mapcar #'make-column new-value) style))

(defmethod format-header ((style  columns-mixin)
                          (stream t))
  (let+ (((&structure-r/o style- columns separator) style)
         (produced-output?)
         (printed-ellipsis?))
    (iter (for  column   in columns)
          (with position =  0)
          (when (column-produces-output? column)
            (when-column-fits (column separator
                                      position produced-output? printed-ellipsis?)
              (when produced-output?
                (format stream separator)
                (incf position (length separator)))
              (format-header column stream)
              (setf produced-output? t)))
          (incf position (column-width column))
          (finally (when produced-output? (terpri stream))))))

(defmethod format-event ((event  t)
                         (style  columns-mixin)
                         (stream t)
                         &key &allow-other-keys)
  (let+ (((&structure-r/o style- columns separator) style)
         (produced-output?)
         (printed-ellipsis?))
    (iter (for  column   in columns)
          (with position =  0)
          (if (column-produces-output? column)
              (when-column-fits (column separator
                                        position produced-output? printed-ellipsis?)
                (when produced-output?
                  (format stream separator)
                  (incf position (length separator)))
                (format-event event column stream)
                (setf produced-output? t))
              (format-event event column stream))
          (incf position (column-width column)))))

(defmethod print-object ((object columns-mixin) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "(~D)" (length (style-columns object)))))

;; Local Variables:
;; coding: utf-8
;; End:

;;;; text-style-mixins.lisp --- Mixin classes for textual formatting style classes.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.formatting)

;;; `periodic-printing-mixin'

(defclass periodic-printing-mixin ()
  ((stream       :type     (or null stream)
                 :accessor style-%stream
                 :initform nil
                 :documentation
                 "Stores the stream that should be used for periodic
                  printing.")
   (pretty-state :type     list
                 :accessor style-%pretty-state
                 :documentation
                 "Stores the pretty-printer state that should be
                  used for periodic printing.")
   (executor     :accessor style-%executor
                 :documentation
                 "Stores the timer used to trigger periodic
                  printing.")
   (lock         :reader   style-%lock
                 :initform (bt:make-recursive-lock
                            "Periodic printing lock")
                 :documentation
                 "Stores a lock that protects timer-triggered
                  accesses to the style object against
                  `format-event'-triggered accesses."))
  (:default-initargs
   :print-interval 1)
  (:documentation
   "This mixin class is intended to be mixed into formatting classes
    that produce output periodically instead of being triggered by the
    arrival of events.

    When `format-event' is called, an :around method prevents
    output-producing methods from running. Instead, these method are
    run a timer-driven way."))

(defmethod initialize-instance :after ((instance periodic-printing-mixin)
                                       &key
                                       print-interval)
  (let+ (((&flet output (style)
            (when-let ((stream (style-%stream style)))
              (let+ (((*print-right-margin* *print-miser-width*)
                      (style-%pretty-state style)))
                (ignore-some-conditions (stream-error)
                  (format-event :trigger style stream)))))))
    (setf (style-%executor instance) (make-instance 'timed-executor/weak
                                                    :interval print-interval
                                                    :function #'output
                                                    :args     (list instance)))))

(defmethod style-print-interval ((style periodic-printing-mixin))
  (executor-interval (style-%executor style)))

(defmethod (setf style-print-interval) :before ((new-value t)
                                                (style     periodic-printing-mixin))
  ;; Validate NEW-VALUE to prevent bad timer scheduling.
  (check-type new-value print-interval))

(defmethod (setf style-print-interval) ((new-value t)
                                        (style     periodic-printing-mixin))
  (setf (executor-interval (style-%executor style)) new-value))

(defmethod collects? ((style periodic-printing-mixin))
  t)

(defmethod format-event :around ((event  t)
                                 (style  periodic-printing-mixin)
                                 (stream t)
                                 &key)
  "Protect against concurrent access to STYLE and store STREAM for use
   in timer-driven output."
  (bt:with-recursive-lock-held ((style-%lock style))
    (unless (eq event :trigger)
      (setf (style-%stream style)       stream
            (style-%pretty-state style) (list *print-right-margin*
                                              *print-miser-width*)))

    (call-next-method)))

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
                                 &rest args &key)
  (write-string
   (with-output-to-string (stream)
     (apply #'call-next-method event style stream args))
   stream)
  (force-output stream))

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
                                 &key)
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
                                 &key)
  "Print a separator before each event."
  (print-separator (style-separator style) stream (or *print-right-margin* 80)))

;; Utility functions

(defun %separator-width (spec &key (max-columns 0))
  (etypecase spec
    (null         0)
    (character    1)
    (string       (length spec))
    (rule-spec    max-columns)
    ((eql :clear) 0)
    (list         (reduce #'+ spec
                          :key (rcurry #'%separator-width  max-columns)))))

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

;;; `max-lines-mixin'

(defclass max-lines-mixin ()
  ((max-lines :initarg  :max-lines
              :type     (or null non-negative-integer)
              :reader   style-max-lines
              :initform nil
              :documentation
              "Nil, indicating no limitation, or the maximum number of
               lines the style should produce for a single event."))
  (:documentation
   "This class is indented to be mixed into style classes which allow
    restricting the amount of output to a given number of lines."))

(defmethod format-event :around ((event  t)
                                 (style  max-lines-mixin)
                                 (target t)
                                 &key)
  (let ((*print-lines* (style-max-lines style)))
    (call-next-method)))

;;; `width-specification-mixin'

(defclass width-specification-mixin ()
  ((widths   :initarg  :widths
             :type     width-specification
             :accessor column-widths
             :documentation
             "Stores a `width-specification' for the column.")
   (priority :initarg  :priority
             :type     positive-real
             :accessor column-priority
             :documentation
             "Stores a real specifying the importance of this column
              in comparison to other columns. Larger priorities
              indicate more important columns."))
  (:default-initargs
   :priority 3)
  (:documentation
   "This class is intended to be mixed into column classes for which a
    width should be automatically computed based on a specification of
    possible widths."))

(defmethod (setf column-widths) :before ((new-value t)
                                         (style     width-specification-mixin))
  (check-type new-value width-specification))

(defmethod shared-initialize :before ((instance   width-specification-mixin)
                                      (slot-names t)
                                      &key
                                      (widths nil widths-supplied?)
                                      (width  nil width-supplied?))
  (unless (or widths-supplied? width-supplied?)
    (missing-required-initarg 'width-specification-mixin
                              :widths-xor-width))
  (when (and widths-supplied? width-supplied?)
    (check-type widths (satisfies width-specification?))
    (unless (compatible-with-width-specification? width widths)
      (incompatible-initargs 'width-specification-mixin
                             :widths widths
                             :width  width))))

(defmethod shared-initialize :after ((instance   width-specification-mixin)
                                     (slot-names t)
                                     &key
                                       (widths nil widths-supplied?)
                                       (width  nil width-supplied?))
  (cond
    (width-supplied?
     (setf (column-widths instance) width))
    (widths-supplied?
     (setf (column-widths instance) widths))))

;;; `width-mixin'

(defclass width-mixin ()
  ((width     :initarg  :width
              :type     (or null non-negative-integer)
              :accessor column-width
              :initform nil
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
                                 &key)
  (call-with-width-limit
   stream (column-width style) (column-alignment style)
   (lambda (stream) (call-next-method event style stream))))

(defmethod print-object ((object width-mixin) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~:[<no width>~;~:*~D~] ~A"
            (column-width object) (column-alignment object))))

;; Utility functions

(defun call-with-width-limit (stream limit align thunk)
  "Call THUNK with a single argument that is a stream. Format things
   printed to the stream by THUNK on STREAM ensuring a width limit
   LIMIT and alignment according to ALIGN. ALIGN can be :left
   or :right."
  (let+ ((value  (with-output-to-string (stream)
                   (funcall thunk stream)))
         (length (length value))
         ((&flet ellipsis ()
            (if *textual-output-can-use-utf-8?* #\… #\.))))
    (cond
      ;; No room at all - print nothing.
      ((zerop limit))

      ;; Only room for a single character - print ellipsis if we have
      ;; any output.
      ((< limit 1 length)
       (format stream "~C" (ellipsis)))

      ;; Not enough room - print value and ellipsis.
      ((< limit length)
       (ecase align
         (:left
          (format stream "~A~C"
                  (subseq value 0 (- limit 1)) (ellipsis)))
         (:right
          (format stream "~C~A"
                  (ellipsis) (subseq value (1+ (- length limit)))))))

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

(defmethod rsb.ep:access? ((processor columns-mixin)
                           (part      t)
                           (mode      t))
  (rsb.ep:access? (style-columns processor) part mode))

(defmethod format-header ((style  columns-mixin)
                          (stream t))
  (let+ (((&structure-r/o style- columns separator) style)
         (produced-output?))
    (iter (for column in columns)
          (when (column-produces-output? column)
            (when produced-output?
              (format stream separator))
            (format-header column stream)
            (setf produced-output? t)))
    (when produced-output? (terpri stream))))

(defmethod format-event ((event  t)
                         (style  columns-mixin)
                         (stream t)
                         &key)
  (let+ (((&structure-r/o style- columns separator) style)
         (produced-output?))
    (iter (for column in columns)
          (if (column-produces-output? column)
              (progn
                (when produced-output?
                  (format stream separator))
                (format-event event column stream)
                (setf produced-output? t))
              (format-event event column stream)))))

(defmethod print-object ((object columns-mixin) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "(~D)" (length (style-columns object)))))

;;; `widths-caching-mixin'

(defclass widths-caching-mixin ()
  ((width-cache    :type     vector
                   :accessor style-width-cache
                   :initform (make-array 1000
                                         :initial-element nil
                                         :adjustable      t)
                   :documentation
                   "Stores results of width computations indexed by
                    target width.")
   (previous-width :type     (or null non-negative-integer)
                   :accessor style-%previous-width
                   :initform nil))
  (:documentation
   "This class is intended to be mixed into classes which perform
    column width computations. It add caching of computation results
    for repeated widths."))

(defmethod style-compute-column-widths ((style   widths-caching-mixin)
                                        (columns sequence)
                                        (width   integer)
                                        &key
                                        separator-width)
  (declare (ignore separator-width))
  (let+ (((&structure-r/o style- width-cache %previous-width) style)
         ((&flet ensure-cached-widths (width)
            (unless (< width (length width-cache))
              (adjust-array width-cache (1+ width) :initial-element nil))
            (or (aref width-cache width)
                (setf (aref width-cache width) (call-next-method))))))
    (multiple-value-prog1
        (values (ensure-cached-widths width) (equal %previous-width width))
      (setf %previous-width width))))

;; Local Variables:
;; coding: utf-8
;; End:

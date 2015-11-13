;;;; style-mixins.lisp --- Mixin classes for formatting style classes.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.formatting)

;;; `counting-mixin'

(defclass counting-mixin ()
  ((count :initarg  :count
          :type     non-negative-integer
          :accessor style-count
          :initform 0
          :documentation
          "Stores the number of output cycles already performed by the
           formatter."))
  (:documentation
   "This class is intended to be mixed into formatter classes that
    need keep track of the number of performed output cycles."))

(defmethod format-event :after ((event  t)
                                (style  counting-mixin)
                                (stream t)
                                &key)
  (incf (style-count style)))

;;; `activity-tracking-mixin'

(defclass activity-tracking-mixin ()
  ((last-activity :initarg  :last-activity
                  :type     (or null local-time:timestamp)
                  :accessor style-last-activity
                  :initform nil
                  :documentation
                  "Unless nil, time of most recent activity on the
                   style object."))
  (:documentation
   "Allows storing the time of the most recent activity on the style
    object."))

(defmethod format-event :around ((event  t)
                                 (style  activity-tracking-mixin)
                                 (stream t)
                                 &key)
  (unless (eq event :trigger)
    (setf (style-last-activity style) (local-time:now)))
  (call-next-method))

;;; `delegating-mixin'

(defclass delegating-mixin ()
  ((sub-styles :initarg  :sub-styles
               :type     list
               :accessor style-sub-styles
               :initform '()
               :documentation
               "Stores predicates and corresponding sub-styles as an
                alist of items of the form

                  (PREDICATE . SUB-STYLE)."))
  (:documentation
   "This class is intended to be used in formatting classes that
    delegate to sub-styles based on dispatch predicates."))

(defmethod sub-style-for ((style delegating-mixin)
                          (event t))
  "Return a list of sub-styles of STYLE whose predicates succeed on
   EVENT."
  (map 'list #'cdr
       (remove-if (complement (rcurry #'funcall event))
                  (style-sub-styles style)
                  :key #'car)))

(defmethod delegate ((event  t)
                     (style  delegating-mixin)
                     (stream t))
  (let+ (((&labels apply-style (style-or-styles)
            (typecase style-or-styles
              (sequence
               (map nil #'apply-style style-or-styles))
              (t
               (format-event event style-or-styles stream))))))
    (map nil #'apply-style (sub-style-for style event))))

(defmethod format-event ((event  t)
                         (style  delegating-mixin)
                         (stream t)
                         &key)
  "Delegate formatting of EVENT on STREAM to appropriate sub-styles of
   STYLE."
  (delegate event style stream))

(defmethod print-object ((object delegating-mixin) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "(~D)" (length (style-sub-styles object)))))

;;; `sub-style-grouping-mixin'

(defclass sub-style-grouping-mixin (delegating-mixin)
  ((key  :initarg  :key
         :type     function
         :accessor style-key
         :initform #'identity
         :documentation
         "Stores a function that is called on events to derive key
          objects which identify the sub-style that should be used for
          the respective events.")
   (test :initarg  :test
         :type     function
         :accessor style-test
         :initform #'eql
         :documentation
         "Stores a function which is used to compare keys when
          searching for the sub-style that should be used for a
          particular events."))
  (:documentation
   "This mixin class add to `delegating-mixin' the ability to
    dynamically create sub-styles and dispatch to these based on
    properties of events.

    Creation of and dispatching to sub-styles is based on the usual
    key/test mechanism for extracting a key from events and testing it
    against previously extracted key. A new sub-style is created and
    added whenever the key extracted from an event does not match the
    key of any previously created sub-styles."))

(defmethod sub-style-for ((style sub-style-grouping-mixin)
                          (event t))
  ;; If there is a sub-style suitable for handling EVENT, dispatch to
  ;; it. Otherwise create a new sub-style and then dispatch to it.
  (or (call-next-method)
      (let ((key (funcall (style-key style) event)))
        (when-let ((sub-style (make-sub-style-entry style key)))
          (push sub-style (style-sub-styles style))
          (call-next-method)))))

(defmethod format-event :around ((event  t)
                                 (style  sub-style-grouping-mixin)
                                 (stream t)
                                 &key)
  (if (eq event :trigger)
      (call-next-method)
      (delegate event style stream)))

;;; `sub-style-sorting-mixin'

(defclass sub-style-sorting-mixin (delegating-mixin)
  ((sort-predicate :initarg  :sort-predicate
                   :type     function
                   :accessor style-sort-predicate
                   :documentation
                   "Stores the predicate according to which sub-styles
                    should be sorted when retrieved as a sorted
                    list.")
   (sort-key       :initarg  :sort-key
                   :type     function
                   :accessor style-sort-key
                   :documentation
                   "Stores the key reader that should be used when
                    sorting sub-styles."))
  (:default-initargs
   :sort-predicate (missing-required-initarg
                    'sub-style-sorting-mixin :sort-predicate)
   :sort-key       (missing-required-initarg
                    'sub-style-sorting-mixin :sort-key))
  (:documentation
   "This mixin adds to delegating formatting style classes the ability
    to retrieve sub-styles sorted according to a predicate."))

(defmethod style-sub-styles/sorted ((style sub-style-sorting-mixin)
                                    &key
                                    (predicate (style-sort-predicate style))
                                    (key       (style-sort-key style)))
  (sort (map 'list #'cdr (style-sub-styles style)) predicate
        :key key))

;;; `sub-style-pruning-mixin'

(defclass sub-style-pruning-mixin ()
  ((prune-predicate :initarg  :prune-predicate
                    :type     (or null function)
                    :accessor style-prune-predicate
                    :initform nil
                    :documentation
                    "Stores a function that is called with a sub-style
                     as its sole argument to determine whether the
                     sub-style should be removed from the list of
                     sub-styles."))
  (:documentation
   "This mixin class adds pruning of dynamically created sub-styles
    based on a prune predicate."))

(defmethod prune-sub-styles ((style sub-style-pruning-mixin))
  (let+ (((&structure style- sub-styles prune-predicate) style))
    (when prune-predicate
      (setf sub-styles (remove-if prune-predicate sub-styles
                                  :key #'cdr)))))

(defmethod format-event :before ((event  (eql :trigger))
                                 (style  sub-style-pruning-mixin)
                                 (stream t)
                                 &key)
  (prune-sub-styles style))

;;; `activity-based-sub-style-pruning-mixin'

(defclass activity-based-sub-style-pruning-mixin (sub-style-pruning-mixin)
  ()
  (:documentation
   "Specialization of `sub-style-pruning-mixin' which constructs a
    prune predicate that compares activity times queried via
    `style-last-activity' against a given threshold."))

(defmethod shared-initialize :after
    ((instance   activity-based-sub-style-pruning-mixin)
     (slot-names t)
     &key
     (prune-after     240                          prune-after-supplied?)
     (prune-predicate (when prune-after
                        (prune-after prune-after)) prune-predicate-supplied?))
  (when (and prune-after-supplied? prune-predicate-supplied?)
    (incompatible-initargs 'activity-based-sub-style-pruning-mixin
                           :prune-after     prune-after
                           :prune-predicate prune-predicate))

  (setf (style-prune-predicate instance) prune-predicate))

;; Utility functions

(defun prune-after (inactive-time)
  "Return a pruning predicate which marks sub-styles for pruning when
   the have been inactive for INACTIVE-TIME seconds."
  (lambda (sub-style)
    (if-let ((activity (style-last-activity sub-style)))
      (> (local-time:timestamp-difference (local-time:now) activity)
         inactive-time)
      t)))

;;; `timestamp-mixin'

(defclass timestamp-mixin ()
  ((timestamp :type     function
              :accessor style-timestamp
              :writer   (setf style-%timestamp)
              :documentation
              "Stores a function which extracts and returns a specific
               timestamp from a given event."))
  (:default-initargs
   :timestamp :send)
  (:documentation
   "This mixins stores a function which extracts a given timestamp
    from events. It is intended to be mixed into style classes which
    extract a configurable timestamp from processed events."))

(defmethod shared-initialize :after ((instance   timestamp-mixin)
                                     (slot-names t)
                                     &key
                                     (timestamp nil timestamp-supplied?))
  (when timestamp-supplied?
    (setf (style-timestamp instance) timestamp)))

(defmethod (setf style-timestamp) ((new-value symbol)
                                   (style     timestamp-mixin))
  (setf (style-%timestamp style)
        (lambda (event)
          (timestamp->unix/nsecs (timestamp event new-value))))
  new-value)

;;; `temporal-bounds-mixin'

(defclass temporal-bounds-mixin ()
  ((lower-bound :initarg  :lower-bound
                :type     time-spec
                :accessor lower-bound
                :initform '(- :now 20)
                :documentation
                "Stores a specification of the lower bound of the
                 temporal interval of interest. See type
                 `time-spec'.")
   (upper-bound :initarg  :upper-bound
                :type     time-spec
                :accessor upper-bound
                :initform :now
                :documentation
                "Stores a specification of the upper bound of the
                 temporal interval of interest. See type
                 `time-spec'."))
  (:documentation
   "This mixin adds lower and upper temporal bounds which can be
    provided in the form of abstract specifications. Realizations of
    these specifications can be retrieved for concrete points in
    time."))

(defmethod shared-initialize :after ((instance   temporal-bounds-mixin)
                                     (slot-names t)
                                     &key)
  (check-bounds-spec (bounds instance)))

(defmethod bounds ((thing temporal-bounds-mixin))
  (list (lower-bound thing) (upper-bound thing)))

(defmethod (setf bounds) :before ((new-value list)
                                  (thing     temporal-bounds-mixin))
  (check-bounds-spec new-value))

(defmethod (setf bounds) ((new-value list)
                          (thing     temporal-bounds-mixin))
  (setf (lower-bound thing) (first  new-value)
        (upper-bound thing) (second new-value)))

(defmethod bounds/expanded ((thing temporal-bounds-mixin)
                            &optional now)
  (%expand-bounds-spec (bounds thing) now))

(defmethod range/expanded ((thing temporal-bounds-mixin))
  (let+ (((lower upper) (bounds/expanded thing 0)))
    (- upper lower)))

;; Utility functions

(defun check-bounds (thing)
  (unless (typep thing 'bounds)
    (error 'type-error
           :datum         thing
           :expected-type 'bounds)))

(defun check-bounds-spec (thing)
  (unless (and (typep thing 'bounds-spec)
               (let+ (((lower upper) (%expand-bounds-spec thing 0))
                      (min           (min lower upper))
                      (lower         (- lower min))
                      (upper         (- upper min)))
                 (typep (list lower upper) 'bounds)))
    (error 'type-error
           :datum         thing
           :expected-type 'bounds-spec)))

(defun %expand-now (now)
  (let ((raw (etypecase now
               (null                 (local-time:now))
               (function             (funcall now))
               (local-time:timestamp now)
               (integer              now))))
    (etypecase raw
      (local-time:timestamp (timestamp->unix/nsecs raw))
      (integer              raw))))

(defun %expand-bounds-spec (spec &optional now)
  "Translate SPEC into two absolute timestamps of type
   `timestamp/unix/nsec' and return a list of the two.

   If the translation requires the current time, NOW is called without
   arguments to retrieve it."
  (let+ (((lower upper) spec)
         now*
         ((&flet now ()
            (or now* (setf now* (%expand-now now))))))
    (values (list (%expand-time-spec lower #'now)
                  (%expand-time-spec upper #'now))
            now*)))

(defun %expand-time-spec (spec now)
  "Translate SPEC into an absolute timestamp of type
   `timestamp/unix/nsec' and return the timestamp.

   If the translation requires the current time, NOW is called without
   arguments to retrieve it."
  (etypecase spec
    ((eql :now)
     (%expand-now now))

    ((cons (member + - * /))
     (apply (first spec)
            (mapcar (rcurry #'%expand-time-spec now) (rest spec))))

    (real
     (floor spec 1/1000000000))))

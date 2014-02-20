;;;; style-mixins.lisp --- Mixin classes for formatting style classes.
;;;;
;;;; Copyright (C) 2011, 2013, 2014 Jan Moringen
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
                                &key &allow-other-keys)
  (incf (style-count style)))

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
                         &key &allow-other-keys)
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
                                 &key &allow-other-keys)
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

(defmethod bounds ((thing temporal-bounds-mixin))
  (list (lower-bound thing) (upper-bound thing)))

(defmethod (setf bounds) ((new-value list)
                          (thing     temporal-bounds-mixin))
  (check-type new-value bounds-spec)

  (setf (lower-bound thing) (first  new-value)
        (upper-bound thing) (second new-value)))

(defmethod bounds/expanded ((thing temporal-bounds-mixin))
  (let+ ((now)
         ((&flet now ()
            (or now
                (setf now (timestamp->unix/nsecs (local-time:now)))))))
    (values (list (%expand-time-spec (lower-bound thing) #'now)
                  (%expand-time-spec (upper-bound thing) #'now))
            (now))))

(defmethod range/expanded ((thing temporal-bounds-mixin))
  (let+ (((lower upper) (bounds/expanded thing)))
    (- upper lower)))

;; Utility functions

(defun %expand-time-spec (spec now)
  "Translate SPEC into an absolute timestamp of type
   `timestamp/unix/nsec' and return the timestamp.

   If the translation requires the current time, NOW is called without
   arguments to retrieve it."
  (etypecase spec
    ((eql :now)
     (funcall now))

    ((cons (member + - * /))
     (apply (first spec)
            (mapcar (rcurry #'%expand-time-spec now) (rest spec))))

    (real
     (floor spec 1/1000000000))))

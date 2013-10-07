;;;; quantity-mixins.lisp --- Mixin classes for quantity classes.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsb.stats)

;;; `named-mixin' mixin class

(defclass named-mixin ()
  ((name :initarg  :name
         :type     string
         :accessor quantity-name
         :initform (missing-required-initarg 'named-mixin :name)
         :documentation
         "Stores the name of the quantity."))
  (:documentation
   "This mixin class is intended to be mixed into quantity classes to
take care of the quantity name."))

(defmethod print-object ((object named-mixin) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A = " (quantity-name object))
    (format-value object stream)))

;;; `extract-function-mixin' mixin class

(defclass extract-function-mixin ()
  ((extractor :initarg  :extractor
              :type     function
              :accessor quantity-extractor
              :initform (missing-required-initarg
                         'extract-function-mixin :extractor)
              :documentation
              "Stores a function that is called to extract a value
from some object."))
  (:documentation
   "This mixin class is intended to be mixed into quantity classes
that should provide flexible extraction of values from events or other
sources."))

(defmethod update! ((quantity extract-function-mixin)
                    (event    event))
  (update! quantity (funcall (quantity-extractor quantity) event)))

;;; `meta-data-mixin' mixin class

(defclass meta-data-mixin (named-mixin)
  ((key          :initarg  :key
                 :type     meta-data-selector
                 :accessor quantity-key
                 :documentation
                 "Stores the key for which meta-data items should be extracted
from processed events.")
   (when-missing :initarg  :when-missing
                 :type     when-missing-policy
                 :accessor quantity-when-missing
                 :initform :skip
                 :documentation
                 "Stores a designator the policy that should be
employed when a meta-data item is not available."))
  (:default-initargs
   :key (missing-required-initarg 'meta-data-mixin :key))
  (:documentation
   "This class is intended to be mixed into quantity classes that
process meta-data items of events."))

(defmethod shared-initialize :around ((instance   meta-data-mixin)
                                      (slot-names t)
                                      &rest args
                                      &key
                                      key
                                      (name (when key (string key))))
  (check-type key meta-data-selector)

  (apply #'call-next-method instance slot-names
         (append (when name (list :name name))
                 (remove-from-plist args :name))))

(defmethod update! ((quantity meta-data-mixin)
                    (event    event))
  (let+ (((&accessors-r/o (key          quantity-key)
                          (when-missing quantity-when-missing)) quantity))
    (case key
      (:keys
       (mapc (curry #'update! quantity) (meta-data-keys event)))
      (:values
       (mapc (curry #'update! quantity) (meta-data-values event)))
      (t
       (let ((value (or (meta-data event key) when-missing)))
         (unless (eq value :skip)
           (update! quantity value)))))))

(defmethod find-quantity-class ((spec (eql :meta-data-moments)))
  (find-class 'meta-data-moments))

;;; `collecting-mixin' mixin class

(defclass collecting-mixin ()
  ((values :initarg  :values
           :type     vector
           :reader   quantity-values
           :initform (make-array 0
                                 :fill-pointer 0
                                 :adjustable   t)
           :documentation
           "Stores the values that have been collected from events."))
  (:documentation
   "This mixin is intended to be added to quantity classes the values
of which are computed by accumulating auxiliary values across multiple
events."))

(defmethod update! ((quantity collecting-mixin)
                    (value    (eql nil)))
  "Ignore nil value by default."
  (values))

(defmethod update! ((quantity collecting-mixin)
                    (value    t))
  "Add VALUE to the collected values."
  (vector-push-extend value (quantity-values quantity))
  (when (next-method-p)
    (call-next-method)))

(defmethod reset! ((quantity collecting-mixin))
  "Clear the collection of values in QUANTITY."
  (setf (fill-pointer (quantity-values quantity)) 0)
  (when (next-method-p)
    (call-next-method)))

;;; `reduction-mixin' mixin class

(defclass reduction-mixin ()
  ((empty-value :initarg  :empty-value
                :accessor quantity-empty-value
                :initform 0
                :documentation
                "This class allocated slot stores a value that should
be produced when the quantity is queried when no values have been
collected.")
   (reduce-by   :initarg  :reduce-by
                :type     function
                :accessor quantity-reduce-by
                :initform #'+
                :documentation
                "Stores the reduce function that produces the value of
the quantity by reducing a collection of values."))
  (:documentation
   "This mixin class is intended to be mixed into quantity classes
which compute the quantity value from a collection of values using a
reduction function."))

(defmethod quantity-value ((quantity reduction-mixin))
  (let+ (((&accessors-r/o
           (empty-value quantity-empty-value)
           (values      quantity-values)
           (reduce-by   quantity-reduce-by)) quantity))
    (reduce reduce-by values
            :initial-value empty-value)))

;;; `all-time-mixin' mixin class

(defclass all-time-mixin (collecting-mixin
                          reduction-mixin)
  ()
  (:documentation
   "This class is intended to be mixed into quantity classes which
continuously aggregate new values into the quantity value. Examples
include size of all transferred data in a whole session."))

(defmethod reset! ((quantity all-time-mixin))
  (setf (quantity-empty-value quantity) (quantity-value quantity))
  (when (next-method-p)
    (call-next-method)))

;;; `moments-mixin' mixin class

(defclass moments-mixin ()
  ()
  (:documentation
   "This mixin class is intended to be mixed into quantity classes
that provided mean and variance of a collection of accumulated
values."))

(defmethod quantity-value ((quantity moments-mixin))
  (let+ (((&accessors-r/o (values quantity-values)) quantity))
    (if (emptyp values)
        (values :n/a          :n/a)
        (values (mean values) (standard-deviation values)))))

(defmethod format-value ((quantity moments-mixin) (stream t))
  (apply #'format stream "~,3F ± ~,3F"
         (multiple-value-list (quantity-value quantity))))

;;; `histogram-mixin' mixin class

(defclass histogram-mixin ()
  ((values :initarg  :values
           :type     hash-table
           :reader   %quantity-values
           :initform (make-hash-table :test #'equalp)
           :documentation
           "Stores a mapping from values in the quantity's domain to
the respective frequencies of these values."))
  (:documentation
   "This mixin class is intended to be mixed into quantity classes
that accumulate values in form of a histogram."))

(defmethod quantity-values ((quantity histogram-mixin))
  (hash-table-values (%quantity-values quantity)))

(defmethod quantity-value ((quantity histogram-mixin))
  (hash-table-alist (%quantity-values quantity)))

(defmethod update! ((quantity histogram-mixin)
                    (value    t))
  (incf (gethash value (%quantity-values quantity) 0)))

(defmethod reset! ((quantity histogram-mixin))
  (clrhash (%quantity-values quantity)))

(defmethod format-value ((quantity histogram-mixin)
                         (stream   t))
  (format stream "~:[N/A~;~:*~{~{~A: ~D~}~^, ~}~]"
          (map 'list (lambda (cons) (list (car cons) (cdr cons)))
               (sort (quantity-value quantity) #'> :key #'cdr))))

;;; `rate-mixin' mixin class

(defclass rate-mixin ()
  ((start-time :initarg  :start-time
               :accessor %quantity-start-time
               :initform nil
               :documentation
               "Stores the start time of the current computation
period."))
  (:documentation
   "This class is intended to be mixed into quantity classes that
compute the rate of a quantity over a period of time. It takes care of
tracking the start and end times of time periods and turns a computed
absolute value into a rate value using this information."))

(defmethod quantity-value :around ((quantity rate-mixin))
  (let+ ((value (call-next-method))
         ((&accessors-r/o (start-time %quantity-start-time)) quantity)
         (now  (local-time:now))
         (diff (when start-time
                 (local-time:timestamp-difference now start-time))))
    (cond
      ;; Start time not recorded yet or no difference to start time.
      ((not (and diff (plusp diff)))
       :n/a)
      ;; No value recorded for current time period.
      ((not (realp value))
       :n/a)
      ;; Everything is fine, compute rate.
      (t
       (/ value diff)))))

(defmethod reset! ((quantity rate-mixin))
  (setf (%quantity-start-time quantity) (local-time:now))
  (when (next-method-p)
    (call-next-method)))

;;; `format-mixin' mixin class

(defclass format-mixin ()
  ((format :initarg  :format
           :type     string
           :accessor quantity-format
           :initform "~A"
           :documentation
           "Stores the format string that should be used to format the
value of the quantity."))
  (:documentation
   "This class is intended to be mixed into quantity classes which
format their value by using `cl:format'. It stores a format string
which it uses in a method on `format-value' to format the value of the
quantity."))

(defmethod format-value ((quantity format-mixin) (stream t))
  (format stream (quantity-format quantity) (quantity-value quantity)))

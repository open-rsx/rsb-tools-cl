;;;; quantities.lisp --- A collection of simple quantities.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.stats)

;;; Simple quantities

(macrolet
    ((define-simple-quantity ((name
                               &rest initargs
                               &key
                               (designator  (make-keyword name))
                               (pretty-name (format nil "~(~A~)" name))
                               &allow-other-keys)
                              super-classes
                              &optional doc)
       (let ((class-name (symbolicate "QUANTITY-" name)))
         `(progn
            (defclass ,class-name (named-mixin
                                   ,@super-classes)
              ()
              (:default-initargs
               :name ,pretty-name
               ,@(remove-from-plist initargs :designator :pretty-name))
              ,@(when doc
                  `((:documentation ,doc))))

            (service-provider:register-provider/class 'quantity ',designator
              :class ',class-name)))))

  (define-simple-quantity (count
                           :extractor (constantly 1)
                           :format    "~:D")
      (extract-function-mixin
       collecting-mixin
       reduction-mixin
       format-mixin)
    "This quantity counts events arriving within a period of time.")

  (define-simple-quantity (count/all-time
                           :pretty-name "Count"
                           :extractor   (constantly 1)
                           :format      "~:D")
      (extract-function-mixin
       all-time-mixin
       format-mixin)
    "This quantity counts all arriving events.")

  (define-simple-quantity (rate
                           :extractor (constantly 1)
                           :format    "~,3F")
      (extract-function-mixin
       collecting-mixin
       reduction-mixin
       rate-mixin
       format-mixin)
    "This quantity measures the event rate by counting the events
     arriving within a period of time.")

  (define-simple-quantity (period-time
                           :extractor (rcurry #'timestamp :create)
                           :order     2
                           :filter    (lambda (x y)
                                        (local-time:timestamp-difference y x)))
      (extract-function-mixin
       filter-mixin
       collecting-mixin
       moments-mixin)
    "This quantity measures the period time based on event
     timestamps. The default behavior consists in using creation
     timestamps.")

  (define-simple-quantity (throughput
                           :extractor (rcurry #'event-size 0)
                           :format    "~,3F")
      (extract-function-mixin
       collecting-mixin
       reduction-mixin
       rate-mixin
       format-mixin)
    "This quantity measures the throughput by accumulating the sizes
     of event payloads over a period of time. Note that it is not
     always possible to determine the size of an event payload and
     that, consequently, the value of this quantity may not reflect
     the actual throughput in some cases.")

  (define-simple-quantity (size
                           :extractor (rcurry #'rsb.stats:event-size nil))
      (extract-function-mixin
       collecting-mixin
       moments-mixin)
    "This quantity measures the size of events over a period of time
     and computes mean and variance of the collected values. Note that
     it is not always possible to determine the size of an event
     payload and that, consequently, the value of this quantity may
     not reflect the actual size statistics in some cases.")

  (define-simple-quantity (size/log
                           :extractor #'rsb.stats:event-size/power-of-2)
      (extract-function-mixin
       histogram-mixin)
    "The value of this measures is a histogram of event sizes -
     rounded to the nearest power of 2 - observed over a period of
     time. When output is produced, the most frequent sizes are
     printed first.")

  (define-simple-quantity (size/all-time
                           :pretty-name "Size"
                           :extractor   (rcurry #'rsb.stats:event-size nil)
                           :format      "~:D")
      (extract-function-mixin
       all-time-mixin
       format-mixin)
    "The value of this quantity consists of the accumulated sizes of
     all events.")

  (define-simple-quantity
      (notification-size
       :extractor (lambda (event)
                    (or (meta-data event :rsb.transport.notification-size)
                        :n/a)))
      (extract-function-mixin
       collecting-mixin
       moments-mixin)
    "This quantity measures the size of notifications over a period of
     time and computes mean and variance of the collected values. Note
     that it is not always possible to determine the size the
     notification through which and events has been transmitted and
     that, consequently, the value of this quantity may not reflect
     the actual size statistics in some cases.")

  (define-simple-quantity (scope
                           :extractor (compose #'scope-string
                                               #'event-scope))
      (extract-function-mixin
       histogram-mixin)
    "The value of this quantity is a histogram of event scopes
     observed over a period of time. When output is produced, the most
     frequent scopes are printed first.")

  (define-simple-quantity (method
                           :extractor #'event-method)
      (extract-function-mixin
       histogram-mixin)
    "The value of this quantity is a histogram of event methods
     observed over a period of time. When output is produced, the most
     frequent methods are printed first.")

  (define-simple-quantity (origin
                           :extractor #'event-origin
                           :key       #'princ-to-string)
      (extract-function-mixin
       histogram-mixin)
    "The value of this quantity is a histogram of event origins
     observed over a period of time. When output is produced, the most
     frequent event origins are printed first.")

  (define-simple-quantity (wire-schema
                           :extractor (lambda (event)
                                        (meta-data event :rsb.transport.wire-schema)))
      (extract-function-mixin
       histogram-mixin)
    "The value of this quantity is a histogram of event wire-schemas
     observed over a period of time. When output is produced, the most
     frequent event origins are printed first.")

  (define-simple-quantity (type
                           :extractor #'event-type/simple)
      (extract-function-mixin
       histogram-mixin)
    "The value of this quantity is a histogram of event types over a
     period of time. When output is produced, the most frequent event
     types are printed first."))

;;; Generic meta-data quantities

(defclass meta-data-moments (meta-data-mixin
                             collecting-mixin
                             moments-mixin)
  ()
  (:documentation
   "This quantity collects the values of a given meta-data item over a
    period of time and computes mean and variance of the collected
    values."))

(service-provider:register-provider/class
 'quantity :meta-data-moments :class 'meta-data-moments)

(defmethod initialize-instance :before ((instance meta-data-moments)
                                        &key
                                        key)
  (when (eq key :keys)
    (error "~@<Value ~S specified for ~S initarg of ~S quantity, but ~
            moments cannot be computed over meta-data keys.~@:>"
           key :key 'meta-data-moments)))

(defmethod update! ((quantity meta-data-moments)
                    (event    string))
  (update! quantity (read-from-string event)))

(defclass meta-data-histogram (meta-data-mixin
                               histogram-mixin)
  ()
  (:documentation
   "The value of this quantity is a histogram of the values of a
    meta-data item extracted from events over a period of time. When
    output is produced, the most frequent values are printed first."))

(service-provider:register-provider/class
 'quantity :meta-data-histogram :class 'meta-data-histogram)

;;; Latency quantity

(defclass latency (named-mixin
                   extract-function-mixin
                   collecting-mixin
                   moments-mixin)
  ((from :initarg  :from
         :type     keyword
         :accessor quantity-from
         :initform (missing-required-initarg 'latency :from)
         :documentation
         "Stores a key of the \"from\" (i.e. earlier) timestamp of the
          pair of timestamps for which the latency should be
          computed.")
   (to   :initarg  :to
         :type     keyword
         :accessor quantity-to
         :initform (missing-required-initarg 'latency :to)
         :documentation
         "Stores a key of the \"to\" (i.e. later) timestamp of the
          pair of timestamps for which the latency should be
          computed."))
  (:documentation
   "This quantity collects the differences between specified
    timestamps and computes the mean and variance of the resulting
    latency values."))

(service-provider:register-provider/class
 'quantity :latency :class 'latency)

(defmethod shared-initialize :around ((instance   latency)
                                      (slot-names t)
                                      &rest args
                                      &key
                                      from
                                      to
                                      (name      (format nil "Latency ~(~A~)-~(~A~)"
                                                         from to))
                                      (extractor (make-extractor from to)))
  (apply #'call-next-method instance slot-names
         :name      name
         :extractor extractor
         (remove-from-plist args :name :extractor)))

(defmethod (setf quantity-from) :after ((new-value t)
                                        (quantity  latency))
  (setf (quantity-extractor quantity)
        (make-extractor new-value (quantity-to quantity))))

(defmethod (setf quantity-to) :after ((new-value t)
                                      (quantity  latency))
  (setf (quantity-extractor quantity)
        (make-extractor (quantity-from quantity) new-value)))

(defmethod print-object ((object latency) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~(~A~) - ~(~A~)"
            (quantity-from object) (quantity-to object))))

(defun make-extractor (from to)
  (lambda (event)
    (when-let ((later   (timestamp event to))
               (earlier (timestamp event from)))
      (local-time:timestamp-difference later earlier))))

;;; Expected quantity

(defclass expected (named-mixin)
  ((target   :accessor quantity-target
             :documentation
             "Stores a quantity the value of which should be checked
              in order to determine whether something seems faulty.")
   (expected :reader   quantity-expected
             :writer   (setf quantity-%expected)
             :documentation
             "Stores a specification of the expected value of the
              TARGET quantity. Such a specification can be
              1. a function
              2. a list of the form

                   (:type TYPE)

              3. a value")
   (check    :type     function
             :accessor quantity-%check
             :documentation
             "Stores a function which implements the check specified
              by EXPECTED."))
  (:default-initargs
   :name     "expected"
   :target   (missing-required-initarg 'expected :target)
   :expected (missing-required-initarg 'expected :expected))
  (:documentation
   "This quantity checks the value of a \"slave quantity\" against a
    supplied specification of acceptable values to determine whether
    an unexpected (normally meaning suspicious or even faulty) value
    has been encountered."))

(service-provider:register-provider/class
 'quantity :expected :class 'expected)

(defmethod shared-initialize :after ((instance   expected)
                                     (slot-names t)
                                     &key
                                     target
                                     (expected nil expected-supplied?))
  (when target
    (setf (quantity-target instance) (make-quantity target)))
  (when expected-supplied?
    (setf (quantity-expected instance) expected)))

(defgeneric (setf quantity-expected) (new-value quantity)
  (:method  ((new-value function) (quantity  expected))
    (setf (quantity-%check quantity) new-value))

  (:method ((new-value cons) (quantity  expected))
    (unless (typep new-value '(cons (eql :type)))
      (return-from quantity-expected
        (call-next-method)))

    (let+ (((&structure quantity- target (check %check)) quantity)
           ((&ign type) new-value))
      (setf check (lambda (value)
                    (if (typep value type)
                        (values t   value)
                        (values nil value
                                (lambda (stream)
                                  (format stream "~@<not a ~S: ~
                                                  ~/rsb.stats:print-quantity-value/~@:>"
                                          type target))))))))

  (:method ((new-value t) (quantity  expected))
    (let+ (((&structure quantity- target (check %check)) quantity))
      (setf check (lambda (value)
                    (if (equal value new-value)
                        (values t   value)
                        (values nil value
                                (lambda (stream)
                                  (format stream "~@<!~S: ~
                                                  ~/rsb.stats:print-quantity-value/~@:>"
                                          new-value target))))))))

  (:method :after ((new-value t) (quantity expected))
    (setf (quantity-%expected quantity) new-value)))

(defmethod quantity-value ((quantity expected))
  (let+ (((&structure-r/o quantity- target (check %check)) quantity))
    (funcall check (quantity-value target))))

(defmethod reset! ((quantity expected))
  (reset! (quantity-target quantity)))

(defmethod update! ((quantity expected) (event t))
  (update! (quantity-target quantity) event))

(defmethod format-value ((quantity expected) (stream t))
  (let+ (((&structure-r/o
           quantity- target ((&values ok? &ign description) value)) quantity))
    (if ok?
        (format-value target stream)
        (funcall description stream))))

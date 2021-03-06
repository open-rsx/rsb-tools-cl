;;;; event-style-timeline.lisp --- Event indicators on a simple timeline.
;;;;
;;;; Copyright (C) 2012, 2013, 2014, 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.formatting)

;;; Class `basic-timeline-style'

(defclass basic-timeline-style (sorted-monitor-style
                                temporal-bounds-mixin
                                timestamp-mixin)
  ()
  (:default-initargs
   :print-interval .5

   :upper-bound    '(+ :now 1))
  (:documentation
   "This class is intended to be used as a superclass for timeline
    formatting style classes."))

(defmethod make-sub-style-entry :around ((style basic-timeline-style)
                                         (value t))
  ;; Propagate temporal bounds and timestamp to sub-style.
  (let+ (((&whole entry &ign . sub-style) (call-next-method)))
    (setf (bounds sub-style)          (bounds style)
          (style-timestamp sub-style) (style-timestamp style))
    entry))

(macrolet
    ((define-delegating-method (name)
       `(flet ((applicable? (new-value thing)
                 (compute-applicable-methods
                  (fdefinition '(setf ,name))
                  (list new-value thing))))

          (defmethod (setf ,name) ((new-value t) ; TODO define elsewhere?
                                   (style     columns-mixin))
            (iter (for column in-sequence (style-columns style))
                  (when (applicable? new-value column)
                    (setf (,name column) new-value))))

          (defmethod (setf ,name) :after ((new-value t)
                                          (style     basic-timeline-style))
            (iter (for (_ . sub-style) in-sequence (style-sub-styles style))
                  (when (applicable? new-value sub-style)
                        (setf (,name sub-style) new-value)))))))

  (define-delegating-method lower-bound)
  (define-delegating-method upper-bound)
  (define-delegating-method bounds)
  (define-delegating-method style-timestamp))

;;; Some concrete timeline styles

(macrolet
    ((define-timeline-style ((kind
                             &rest initargs
                             &key &allow-other-keys)
                            &body doc-and-column-specs)
       (let+ ((spec       (format-symbol :keyword  "~A/~A"
                                         :timeline kind))
              (class-name (format-symbol *package* "~A/~A"
                                         :style-timeline kind))
              ((&values column-specs nil documentation)
               (parse-body doc-and-column-specs :documentation t))
              (columns column-specs))
         `(progn
            (defclass ,class-name (basic-timeline-style)
              ()
              (:default-initargs
               :default-columns (list ,@columns)
               ,@initargs)
              ,@(when documentation
                      `((:documentation ,documentation))))

            (service-provider:register-provider/class
             'style ,spec :class ',class-name)))))

  (define-timeline-style (scope :key #'event-scope :test #'scope=)
    "This formatting style indicates the points in time at which
     events occur as dots on a timeline. Separate \"lanes\" which
     share a common timeline are dynamically allocated as events
     occur. Events are grouped by scope."
    (lambda (value)
      (list :constant
            :name      "Scope"
            :value     value
            :formatter (lambda (value stream)
                         (write-string (scope-string value) stream))
            :widths    '(:range 24)
            :priority  2
            :alignment :left))
    (list :timeline))

  (define-timeline-style (origin :key #'event-origin :test #'uuid:uuid=)
    "This formatting style indicates the points in time at which
     events occur as dots on a timeline. Separate \"lanes\" which
     share a common timeline are dynamically allocated as events
     occur. Events are grouped by origin."
    (lambda (value)
      (list :constant
            :name      "Origin"
            :value     value
            :widths    '(:range 8)
            :priority  2
            :alignment :left))
    (list :timeline)))

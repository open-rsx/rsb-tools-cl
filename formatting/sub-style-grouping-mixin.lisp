;;;; sub-style-grouping-mixin.lisp --- Mixin for grouping events into sub-styles.
;;;;
;;;; Copyright (C) 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.formatting)

(defclass sub-style-grouping-mixin (delegating-mixin)
  ((key  :initarg  :key
         :type     function
         :accessor style-key
         :initform #'identity
         :documentation
         "Stores a function that is called on events to derive a key
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
      (let ((key (style-key style)))
        (when-let ((sub-style (make-sub-style-entry style (funcall key event))))
          (push sub-style (style-sub-styles style))
          (call-next-method)))))

(defmethod format-event :around ((event  t)
                                 (style  sub-style-grouping-mixin)
                                 (stream t)
                                 &key &allow-other-keys)
  (if (eq event :trigger)
      (call-next-method)
      (delegate event style stream)))

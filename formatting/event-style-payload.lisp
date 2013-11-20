;;;; event-style-payload.lisp --- Formatting style that only processes the payload.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.formatting)

(defmethod find-style-class ((spec (eql :payload)))
  (find-class 'style-payload))

(defclass style-payload (separator-mixin)
  ((payload-style :reader   style-payload-style
                  :writer   (setf style-%payload-style)
                  :documentation
                  "Stores a style instance which should be used to
                   format payloads."))
  (:documentation
   "Only format the payload of each event, but not the meta-data."))

(defmethod shared-initialize :after ((instance   style-payload)
                                     (slot-names t)
                                     &key
                                     (payload-style :raw))
  (setf (style-%payload-style instance)
        (case payload-style
          (:raw payload-style)
          (t    (make-style payload-style)))))

(defmethod format-event ((event  t)
                         (style  style-payload)
                         (stream t)
                         &key &allow-other-keys)
  (format-payload event (style-payload-style style) stream))

(defmethod format-event ((event  event)
                         (style  style-payload)
                         (stream t)
                         &key &allow-other-keys)
  (format-payload (event-data event) (style-payload-style style) stream))

(defmethod format-event :after ((event  t)
                                (style  style-payload)
                                (stream t)
                                &key &allow-other-keys)
  (force-output stream))

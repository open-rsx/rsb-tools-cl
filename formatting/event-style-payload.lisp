;;;; event-style-payload.lisp --- Formatting style that only print the payload.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsb.formatting)

(defmethod find-style-class ((spec (eql :payload)))
  (find-class 'style-payload))

(defclass style-payload (separator-mixin)
  ()
  (:documentation
   "Only format the payload of each event, but not the meta-data."))

(defmethod format-event ((event  t)
                         (style  style-payload)
                         (stream t)
                         &key &allow-other-keys)
  (format-payload event :raw stream))

(defmethod format-event ((event  event)
                         (style  style-payload)
                         (stream t)
                         &key &allow-other-keys)
  (format-payload (event-data event) :raw stream))

(defmethod format-event :after ((event  t)
                                (style  style-payload)
                                (stream t)
                                &key &allow-other-keys)
  (force-output stream))

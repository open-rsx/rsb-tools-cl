;;;; event-style-payload.lisp --- Formatting style that only processes the payload.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.formatting)

(defclass style-payload (separator-mixin
                         max-lines-mixin
                         output-forcing-mixin
                         payload-style-mixin)
  ()
  (:default-initargs
   :payload-style :payload-generic/pretty)
  (:documentation
   "Only format the payload of each event, but not the meta-data."))

(service-provider:register-provider/class
 'style :payload :class 'style-payload)

(defmethod format-event ((event  event)
                         (style  style-payload)
                         (stream t)
                         &rest args &key)
  (pprint-logical-block (stream (list event))
    (apply #'call-next-method event style stream args)))

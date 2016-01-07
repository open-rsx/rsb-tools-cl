;;;; event-style-discard.lisp --- A style that ignores all events.
;;;;
;;;; Copyright (C) 2011, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.formatting)

(defclass style-discard ()
  ()
  (:documentation
   "Ignore all events."))

(service-provider:register-provider/class
 'style :discard :class 'style-discard)

(defmethod format-event ((event t) (style style-discard) (stream t)
                         &key &allow-other-keys)
  (values))

;;;; event-style-discard.lisp --- A style that ignores all events.
;;;;
;;;; Copyright (C) 2011, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.formatting)

(defmethod find-style-class ((spec (eql :discard)))
  (find-class 'style-discard))

(defclass style-discard ()
  ()
  (:documentation
   "Ignore all events."))

(defmethod format-event ((event t) (style style-discard) (stream t)
                         &key &allow-other-keys)
  (values))

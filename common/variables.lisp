;;;; variables.lisp --- Variables used in the cl-rsb-common system.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.common)

;;; Output-related variables

(declaim (special *info-output*))

(defvar *info-output* *standard-output*
  "Stream to which status and information messages should be sent.")

;;; Pre-defined filters

(defparameter *only-user-events-filter*
  (rsb.filter:filter `(:not (:scope :scope ,rsb:*reserved-scope*)))
  "A filter which filters implementation events.")

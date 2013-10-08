;;;; variables.lisp --- Variables used in the cl-rsb-common system.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsb.common)


;;; Output-related variables
;;

(declaim (special *info-output*))

(defvar *info-output* *standard-output*
  "Stream to which status and information messages should be sent.")

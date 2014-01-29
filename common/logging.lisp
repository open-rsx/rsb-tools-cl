;;;; logging.lisp --- Logging-related functions.
;;;;
;;;; Copyright (C) 2011, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.common)

;;; Logging configuration

(defun (setf log-level) (level)
  "Set log level LEVEL."
  (log:config :thread :ndc level))

;;; Utility macros

(defmacro with-logged-warnings (&body body)
  "Execute BODY with unhandled warnings translated to log messages
   with warning category."
  `(handler-bind
       ((warning (lambda (condition)
                   (log:warn "~A: ~A" (type-of condition) condition)
                   (muffle-warning))))
     ,@body))

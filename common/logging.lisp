;;;; logging.lisp --- Logging-related functions.
;;;;
;;;; Copyright (C) 2011, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsb.common)


;;; Logging configuration
;;

(defun make-output-spec ()
  "Return a suitable log5 output specification."
  '("[" log5:category log5:context "]" log5::indent log5:message))

(defun enable-logging (name level
		       &key
		       (stream *standard-output*))
  "Enable logging to STREAM for the log5 category LEVEL."
  (log5:start-stream-sender name stream
			    :output-spec   (make-output-spec)
			    :category-spec level))

(defun (setf log-level) (level
			 &key
			 stream)
  "Set log level LEVEL."
  (when (member level '(:trace :info :warn :error :fatal))
    (enable-logging :errors
		    (ecase level
		      ((:trace :info :warn) 'log5:warn+)
		      (:error               'log5:error+)
		      (:fatal               'log5:error+))
		    :stream (or stream *error-output*)))
  (when (member level '(:trace :info))
    (enable-logging :infos
		    (ecase level
		      (:info  'log5:info)
		      (:trace '(or log5:info log5:trace)))
		    :stream (or stream *standard-output*))))


;;; Utility macros
;;

(defmacro with-logged-warnings (&body body)
  "Execute BODY with unhandled warnings translated to log messages
with warning category."
  `(handler-bind
       ((warning #'(lambda (condition)
		     (log1 :warn "~A" condition)
		     (muffle-warning))))
     ,@body))

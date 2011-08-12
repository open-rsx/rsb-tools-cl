;;; logging.lisp --- Logging-related functions.
;;
;; Copyright (C) 2011 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This Program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This Program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses>.

(in-package :rsb.common)


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

;;; payload-collections.lisp --- Format event collections.
;;
;; Copyright (C) 2012 Jan Moringen
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

(cl:in-package :rsb.formatting)

;;; TODO(jmoringe, 2012-02-06): hack
#.(asdf:load-system :usocket)

(defvar *by-scope-formatting-converter*
  (append (rsb:default-converters) '((t . :fundamental-null)))
  "The converter that should be applied to inner payloads in event
collection payloads.")

(defvar *by-scope-formatting-event-style*
  (make-instance (find-style-class :detailed)
		 :separator nil)
  "The formatting style used for sub-events contained in collections
of events.")

(defmethod format-payload ((data   rsb.protocol:notification)
			   (style  t)
			   (stream stream)
			   &key &allow-other-keys)
  ;;; TODO(jmoringe, 2012-02-05): there should be a dedicated serialization
  (handler-case
      (let ((event (rsb.transport.socket::notification->event
		    *by-scope-formatting-converter* data)))
	(format-event event *by-scope-formatting-event-style* stream))
    (error (condition)
      (pprint-logical-block (stream nil
				    :per-line-prefix "| ")
	(format stream "Failed to decode notification~2&~@<  ~@;~A~:@>"
		(with-output-to-string (stream)
		  (describe data stream)))
	(maybe-print-cause stream condition)))))

(defmethod format-payload ((data   rsb.protocol.collections:events-by-scope-map/scope-set)
			   (style  t)
			   (stream stream)
			   &key &allow-other-keys)
  (let+ (((&accessors-r/o
	   (scope         rsb.protocol.collections:events-by-scope-map/scope-set-scope)
	   (notifications rsb.protocol.collections:events-by-scope-map/scope-set-notifications))
	  data))
    (with-indented-section (stream (format nil "Scope ~S"
					   (sb-ext:octets-to-string scope))
				   :final-fresh-line? nil)
      (iter (for notification each notifications)
	    (unless (first-iteration-p)
	      (fresh-line stream))
	    (format-payload notification style stream)))))

(defmethod format-payload ((data   rsb.protocol.collections:events-by-scope-map)
			   (style  t)
			   (stream stream)
			   &key &allow-other-keys)
  (let ((sets (rsb.protocol.collections:events-by-scope-map-sets data)))
    (with-indented-section (stream (format nil "Events by Scope (~D Scope~:P)"
					   (length sets))
				   :final-fresh-line? nil)
      (iter (for set each sets)
	    (unless (first-iteration-p)
	      (fresh-line stream))
	    (format-payload set style stream)))))

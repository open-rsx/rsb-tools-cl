;;; event-style-meta-data.lisp --- Meta-data-only formatting style class.
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

(in-package :rsb.formatting)

(defmethod find-style-class ((spec (eql :meta-data)))
  (find-class 'style-meta-data))

(defclass style-meta-data (separator-mixin)
  ((routing-info? :initarg  :routing-info?
		  :type     boolean
		  :accessor style-routing-info?
		  :initform t
		  :documentation
		  "Should routing information like destination scope,
origin id and method be printed?")
   (timestamps?   :initarg  :timestamps?
		  :type     boolean
		  :accessor style-timestamps?
		  :initform t
		  :documentation
		  "Should event timestamps be printed?")
   (user-items?   :initarg  :user-items?
		  :type     boolean
		  :accessor style-user-items?
		  :initform t
		  :documentation
		  "Should the dictionary of client-supplied key-value
pairs be printed?")
   (causes?       :initarg  :causes?
		  :type     boolean
		  :accessor style-causes?
		  :initform t
		  :documentation
		  "Should the causes of the event be printed"))
  (:documentation
   "Format the meta-data of each event on multiple lines, but do not
format event payloads."))

(defmethod format-event ((event  event)
			 (style  style-meta-data)
			 (stream t)
			 &key
			 (max-lines 12)
			 &allow-other-keys)
  (bind (((:accessors-r/o (routing-info? style-routing-info?)
			  (timestamps?   style-timestamps?)
			  (user-items?   style-user-items?)
			  (causes?       style-causes?)) style)
	 ((:accessors-r/o (meta-data meta-data-alist)
			  (causes    event-causes)) event)
	 (*print-lines* max-lines))
    ;; Envelope information.
    (when routing-info?
      (with-indented-section (stream "Event")
	(format-pairs/plist
	 stream
	 :scope           (scope-string (event-scope event))
	 :id              (event-id              event)
	 :sequence-number (event-sequence-number event)
	 :origin          (event-origin          event)
	 :method          (event-method          event))))

    ;; Framework and user timestamps.
    (when (and timestamps? (> max-lines 5))
      (with-indented-section (stream "Timestamps")
	(let ((keys (append '(:create :send :receive :deliver)
			    (set-difference
			     (timestamp-keys event)
			     '(:create :send :receive :deliver)))))
	  (format-aligned-items
	   stream keys (map 'list (curry #'timestamp event) keys)))))

    ;; Meta-data.
    (when (and user-items? meta-data (> max-lines 10))
      (with-indented-section (stream "Meta-Data")
	(format-aligned-items/alist stream meta-data)))

    ;; Causes
    (when (and causes? causes)
      (with-indented-section (stream "Causes")
	(format stream "~{~A~^~&~}"
		(map 'list #'event-id->uuid causes))))))

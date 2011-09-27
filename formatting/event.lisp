;;; event.lisp --- Formatting functions for events.
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

(defmethod format-event :around ((event event) (style t) (stream t)
				 &key
				 (max-lines   16)
				 (max-columns 79))
  (let ((*print-right-margin* most-positive-fixnum)
	(*print-miser-width*  most-positive-fixnum))
    (call-next-method event style stream
		      :max-lines   max-lines
		      :max-columns max-columns)))

(defmethod find-style-class ((spec (eql :discard)))
  (find-class 'discard))

(defclass discard ()
  ()
  (:documentation
   "Ignore all events."))

(defmethod format-event ((event event) (style discard) (stream t)
			 &key &allow-other-keys)
  (values))

(defmethod find-style-class ((spec (eql :compact)))
  (find-class 'compact))

(defclass compact ()
  ()
  (:documentation
   "Format each event on a single line."))

(defmethod format-event ((event event) (style compact) (stream t)
			 &key &allow-other-keys)
  (let ((*print-right-margin* most-positive-fixnum)
	(*print-miser-width*  most-positive-fixnum))
    (format stream "~A ~:[ORIGIN? ~;~:*~/rsb::print-id/~] ~A~%"
	    (local-time:now) (event-origin event) event)))

(defmethod find-style-class ((spec (eql :detailed)))
  (find-class 'detailed))

(defclass detailed ()
  ()
  (:documentation
   "Format each with as many details as possible."))

(defmethod format-event ((event event) (style detailed) (stream t)
			 &key
			 max-lines
			 max-columns)
  (bind (((:accessors-r/o (data      event-data)
			  (meta-data meta-data-alist)) event))
   ;; Envelope information.
   (with-indented-section (stream "Event")
     (format-pairs/plist
      stream
      :scope           (scope-string (event-scope event))
      :id              (event-id              event)
      :sequence-number (event-sequence-number event)
      :origin          (event-origin          event)
      :method          (event-method          event)
      :type            (event-type            event)))

   ;; Framework and user timestamps.
   (when (> max-lines 5)
    (with-indented-section (stream "Timestamps")
      (let ((keys (append '(:create :send :receive :deliver)
			  (set-difference
			   (timestamp-keys event)
			   '(:create :send :receive :deliver)))))
	(format-aligned-items
	 stream keys (map 'list (curry #'timestamp event) keys)))))

   ;; Meta-data.
   (when (and meta-data (> max-lines 10))
     (with-indented-section (stream "Meta-Data")
       (format-aligned-items/alist stream meta-data)))

   ;; Payload.
   (when (> max-lines 11)
    (with-indented-section (stream (format nil "Payload (~S)" (class-name (class-of data))))
      (format-payload data :any stream
		      :max-lines   (- max-lines 11)
		      :max-columns (- max-columns 2))))))

(defmethod format-event :after ((event event) (style detailed) (stream t)
				&key
				max-columns
				&allow-other-keys)
  "When formatting events in :detailed style, print a vertical rule
after each event."
  (format stream "~A~%" (make-string max-columns :initial-element #\-)))

(defmethod find-style-class ((spec (eql :payload)))
  (find-class 'payload))

(defclass payload ()
  ()
  (:documentation
   "Only format the payload of each event, but not the meta-data."))

(defmethod format-event ((event event) (style payload) (stream t)
			 &key &allow-other-keys)
  (format-payload (event-data event) :raw stream)
  (force-output stream))

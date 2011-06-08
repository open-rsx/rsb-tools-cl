;;; event.lisp ---
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

(defmethod format-event ((event event) (style (eql :line)) (stream t))
  "Format EVENT on STREAM on a single line."
  (let ((*print-right-margin* most-positive-fixnum)
	(*print-miser-width*  most-positive-fixnum))
    (format stream "[~8,3F ~/rsb::print-id/ -> ~12A] ~A~%"
	    (float (/ (get-internal-real-time) internal-time-units-per-second)) ;; TODO
	    (if (slot-boundp event 'rsb::origin)
		(event-origin event)
		:origin?)
	    (scope-string (event-scope event))
	    event)))

(defmethod format-event ((event event) (style (eql :detailed)) (stream t))
  (format-event event :line stream)


  (with-indent (stream)
    (format stream "Scope  ~A~%" (scope-string (event-scope event)))
    (format stream "ID     ~:/rsb::print-id/~%" (event-id event))
    (format stream "Type   ~A~%" (event-type event))
    (format stream "Origin ~:/rsb::print-id/~%" (ignore-errors (event-origin event))))

  (with-indented-section (stream "Timestamps")
    (let ((keys (append '(:create :send :receive :deliver)
			(set-difference
			 (timestamp-keys event)
			 '(:create :send :receive :deliver)))))
      (format-aligned-items
       stream keys (map 'list (curry #'timestamp event) keys))))

  (when (event-meta-data event)
    (with-indented-section (stream "Meta-Data")
      (format-aligned-items/alist stream (meta-data-alist event))))

  (with-indented-section (stream (format nil "Payload (~A)"
					 (type-of (event-data event))))
    (format-payload (event-data event) :any stream)))

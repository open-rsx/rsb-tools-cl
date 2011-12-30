;;; event-style-detailed.lisp --- Detailed event formatting style class.
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

(cl:in-package :rsb.formatting)

(defmethod find-style-class ((spec (eql :detailed)))
  (find-class 'style-detailed))

(defclass style-detailed (style-meta-data)
  ()
  (:documentation
   "Format each event on multiple lines with as many details as
possible."))

(defmethod format-event ((event  event)
			 (style  style-detailed)
			 (stream t)
			 &key
			 (max-lines   16)
			 (max-columns (or *print-right-margin* 80)))
  ;; Meta-data.
  (call-next-method)

  ;; Payload.
  (bind (((:accessors-r/o (data event-data)) event))
    (when (> max-lines 11)
      (with-indented-section (stream (format nil "Payload (~S)"
					     (class-name (class-of data))))
	(format-payload data :any stream
			:max-lines   (- max-lines 11)
			:max-columns (- max-columns 2))))))

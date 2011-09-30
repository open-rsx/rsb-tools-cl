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

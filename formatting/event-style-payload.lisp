;;; event-style-payload.lisp --- Formatting style that only print the payload.
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

(defmethod find-style-class ((spec (eql :payload)))
  (find-class 'style-payload))

(defclass style-payload (separator-mixin)
  ()
  (:documentation
   "Only format the payload of each event, but not the meta-data."))

(defmethod format-event ((event  event)
			 (style  style-payload)
			 (stream t)
			 &key &allow-other-keys)
  (format-payload (event-data event) :raw stream)
  (force-output stream))

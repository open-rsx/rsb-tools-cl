;;; util.lisp --- Extractor utility functions.
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

(cl:in-package :rsb.stats)

(defun event-size (event &optional (replacement-value :n/a))
  "Try to determine and return the size of the payload of EVENT in
bytes. Return REPLACEMENT-VALUE, if the size cannot be determined."
  (or (meta-data event :rsb.payload-size)
      (let ((data (event-data event)))
	(typecase data
	  (integer
	   (ceiling (integer-length data) 8))
	  (sequence
	   (length data))
	  (t
	   replacement-value)))))

(defun event-size/power-of-2 (event)
  "Like `event-size', but the returned size is rounded to the nearest
power of two, if it is a real number."
  (let ((size (event-size event)))
    (cond
      ((not (realp size))
       size)
      ((zerop size)
       0)
      (t
       (ash 1 (ceiling (log size 2)))))))

(defun event-type/simple (event)
  "Return an object designating the type of EVENT."
  (or (meta-data event :rsb.wire-schema)
      (type-of (event-data event))))

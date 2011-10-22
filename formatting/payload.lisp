;;; payload.lisp --- Formatting methods for different kinds of event payloads.
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

(defmethod format-payload :around ((payload t) (style t) (stream t)
				   &key
				   (max-lines   4)
				   (max-columns 80)) ;;; TODO(jmoringe): default from clon?
  "Supply payload-independent defaults for MAX-LINES and MAX-COLUMNS."
  (let ((*print-lines*        max-lines)
	(*print-right-margin* max-columns)
	(*print-miser-width*  most-positive-fixnum))
    (call-next-method payload style stream
		      :max-lines   max-lines
		      :max-columns max-columns)))

(defmethod format-payload ((payload t) (style t) (stream t)
			   &key
			   max-lines
			   max-columns)
  "Default behavior is to print PAYLOAD using the Lisp printer."
  (let ((*print-miser-width*  (floor max-columns 2))
	(*print-right-margin* max-columns))
    (format stream "~A" payload)))

(defmethod format-payload ((payload vector) (style t) (stream t)
			   &key
			   max-lines
			   max-columns)
  "Format PAYLOAD in form of a hexdump if it is an octet-vector."
  (if (typep payload 'octet-vector)
      (format-octet-vector stream payload)
      (call-next-method)))

(defmethod format-payload ((payload string) (style t) (stream t)
			   &key
			   max-lines
			   max-columns)
  "Format PAYLOAD as a multi-line string, trying to honor MAX-LINES
and MAX-COLUMNS constraints."
  (format-string stream payload))

(defmethod format-payload ((payload standard-object) (style t) (stream t)
			   &key
			   max-lines
			   max-columns)
  "Recursively format the slot values of PAYLOAD."
  (format-instance stream payload))

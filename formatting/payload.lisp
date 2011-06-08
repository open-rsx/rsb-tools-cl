;;; formatting.lisp ---
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


;;;
;;

(defmethod format-payload ((payload vector) (style t) (stream t))
  "TODO(jmoringe): document"
  (if (typep payload 'octet-vector)
      (format stream "~@<~{~(~2,'0X~)~^ ~}~:[~; ..~]~:@>~%"
	      (coerce (subseq payload 0 (min 200 (length payload))) 'list)
	      (> (length payload) 200))
      (call-next-method)))

(defmethod format-payload ((payload string) (style t) (stream t))
  "TODO(jmoringe): document"
  (format stream "~@<~A~:[~; ..~]~:@>~%"
	  (subseq payload 0 (min 200 (length payload)))
	  (> (length payload) 200)))

(defmethod format-payload ((payload standard-object) (style t) (stream t))
  "TODO(jmoringe): document"
  (format-instance stream payload))

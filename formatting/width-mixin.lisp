;;; width-mixin.lisp --- Mixin for width limited and aligned formatting.
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

(defclass width-mixin ()
  ((width     :initarg  :width
	      :type     positive-integer
	      :accessor column-width
	      :initform 16
	      :documentation
	      "Stores the maximum acceptable output width for the
formatter instance.")
   (alignment :initarg  :alignment
	      :type     '(member :left :right)
	      :accessor column-alignment
	      :initform :right
	      :documentation
	      "Stores the alignment that should be employed by the
formatter instance."))
  (:documentation
   "This class is intended to be mixed into formatting classes that
should produce output of a fixed width."))

(defmethod format-event :around ((event  t)
				 (style  width-mixin)
				 (stream t)
				 &key &allow-other-keys)
  (invoke-with-width-limit
   stream (column-width style) (column-alignment style)
   #'(lambda (stream)
       (call-next-method event style stream))))

(defmethod print-object ((object width-mixin) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~D ~A"
	    (column-width object) (column-alignment object))))


;;; Utility functions
;;

(defun invoke-with-width-limit (stream limit align thunk)
  "Call THUNK with a single argument that is a stream. Format things
printed to the stream by THUNK on STREAM ensuring a width limit LIMIT
and alignment according to ALIGN. ALIGN can be :left or :right."
  (let* ((value  (with-output-to-string (stream)
		   (funcall thunk stream)))
	 (length (length value)))
    (cond
      ((zerop limit))
      ((< limit 1 length)
       (format stream "…"))
      ((< limit length)
       (ecase align
	 (:left
	  (format stream "~A…" (subseq value 0 (- limit 1))))
	 (:right
	  (format stream "…~A" (subseq value (1+ (- length limit)))))))
      (t
       (ecase align
	 (:left
	  (format stream "~VA" limit value))
	 (:right
	  (format stream "~V@A" limit value)))))))

(defmacro with-width-limit ((stream-var limit align) &body body)
  "Execute BODY with a STREAM-VAR bound to a stream. Format things
printed to the value of STREAM-VAR in BODY on the previous value of
STREAM-VAR ensuring a width limit LIMIT and alignment according to
ALIGN. ALIGN can be :left or :right."
  `(invoke-with-width-limit ,stream-var ,limit ,align
			    #'(lambda (,stream-var) ,@body)))

;; Local Variables:
;; coding: utf-8
;; End:

;;;; width-mixin.lisp --- Mixin for width limited and aligned formatting.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsb.formatting)

(defclass width-mixin ()
  ((width     :initarg  :width
              :type     positive-integer
              :accessor column-width
              :initform 16
              :documentation
              "Stores the maximum acceptable output width for the
formatter instance.")
   (alignment :initarg  :alignment
              :type     (member :left :right)
              :accessor column-alignment
              :initform :right
              :documentation
              "Stores the alignment that should be employed by the
formatter instance."))
  (:documentation
   "This class is intended to be mixed into formatting classes that
should produce output of a fixed width."))

(defmethod format-header :around ((column width-mixin)
                                  (stream t))
  (invoke-with-width-limit
   stream (column-width column) :left
   #'(lambda (stream)
       (call-next-method column stream))))

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

(defun invoke-with-width-limit (stream limit align thunk)
  "Call THUNK with a single argument that is a stream. Format things
printed to the stream by THUNK on STREAM ensuring a width limit LIMIT
and alignment according to ALIGN. ALIGN can be :left or :right."
  (let* ((value  (with-output-to-string (stream)
                   (funcall thunk stream)))
         (length (length value)))
    (cond
      ;; No room at all - print nothing.
      ((zerop limit))

      ;; Only room for a single character - print ellipsis if we have
      ;; any output.
      ((< limit 1 length)
       (format stream "…"))

      ;; Not enough room - print value and ellipsis.
      ((< limit length)
       (ecase align
         (:left
          (format stream "~A…" (subseq value 0 (- limit 1))))
         (:right
          (format stream "…~A" (subseq value (1+ (- length limit)))))))

      ;; Enough room - pad value.
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

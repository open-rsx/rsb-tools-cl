;;;; image-output-mixin.lisp --- Format event payloads as images.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsb.formatting)

(defclass image-output-mixin ()
  ((width  :type     dimension-spec/full
           :accessor style-width
           :initform t
           :documentation
           "Stores the desired width of output images produced by the
style instance.")
   (height :type     dimension-spec/full
           :accessor style-height
           :initform t
           :documentation
           "Stores the desired height of output images produced by the
style instance."))
  (:documentation
   "This formatting style outputs image data in common image formats
for event payloads consisting of image data."))

(defmethod shared-initialize :after ((instance   image-output-mixin)
                                     (slot-names t)
                                     &key
                                     width
                                     height)
  (when width
    (check-type width dimension-spec)
    (setf (style-width instance) (normalize-dimension-spec width)))
  (when height
    (check-type height dimension-spec)
    (setf (style-height instance) (normalize-dimension-spec height))))

(defmethod print-object ((object image-output-mixin) stream)
  (let+ (((&flet spec-for-format (spec)
            (if (eq spec t) '(:keep) (reverse spec)))))
    (print-unreadable-object (object stream :type t :identity t)
      (format stream "~{~(~A~)~^ ~} x ~{~(~A~)~^ ~}"
              (spec-for-format (style-width object))
              (spec-for-format (style-height object))))))

;;; Utility functions

(declaim (ftype (function (dimension-spec) dimension-spec/full)
                normalize-dimension-spec))

(defun normalize-dimension-spec (spec)
  "Expand the possibly abbreviated dimension specification SPEC into a
full dimension specification."
  (etypecase spec
    (positive-integer
     (list :px spec))
    (positive-real
     (list :%  spec))
    (dimension-spec/full
     spec)))

(declaim (ftype (function (positive-integer dimension-spec/full)
                          positive-integer)
                apply-dimension-spec))

(defun apply-dimension-spec (input spec)
  "Compute output dimension by applying SPEC to INPUT."
  (etypecase spec
    ((eql t)
     input)
    ((cons (eql :px) (cons positive-integer null))
     (second spec))
    ((cons (eql :%)  (cons positive-real null))
     (values (floor (* input 1/100 (second spec)))))))

(declaim (ftype (function (positive-integer dimension-spec/full)
                          (values positive-integer positive-integer))
                apply-dimension-spec/integer-factor))

(defun apply-dimension-spec/integer-factor (input spec)
  "Compute output dimension by applying SPEC to INPUT and ensuring
that the result is an integer fraction of INPUT. Return the output
dimension and the integer scaling factor."
  (let* ((requested (apply-dimension-spec input spec))
         (scale     (round (/ input requested))))
    (values (floor input scale) scale)))

(declaim (inline %yuv422->rgba)
         (ftype (function ((simple-array (unsigned-byte 8) (*)) fixnum
                           (simple-array (unsigned-byte 8) (*)) fixnum)
                          (values))
                %yuv422->rgba))

(defun %yuv422->rgba (yuv-array yuv-start rgb-array rgb-start)
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0) (compilation-speed 0)))

  (let* ((c     (- (aref yuv-array (+ yuv-start 0))  16))
         (d     (- (aref yuv-array (+ yuv-start 1)) 128))
         (c2    (- (aref yuv-array (+ yuv-start 2))  16))
         (e     (- (aref yuv-array (+ yuv-start 3)) 128))

         (valc1 (+ (* 298 c)  128))
         (valc2 (+ (* 298 c2) 128))

         (blue  (* 517 d))
         (green (- (* -208 d) (* 100 e)))
         (red   (* 409 e)))
    (macrolet
        ((store (offset form)
           `(setf (aref rgb-array (+ rgb-start ,offset))
                  (clamp (ash ,form -8) 0 255))))
      (store 0 (+ valc1 red))
      (store 1 (+ valc1 green))
      (store 2 (+ valc1 blue))
      (store 3 65535)
      (store 4 (+ valc2 red))
      (store 5 (+ valc2 green))
      (store 6 (+ valc2 blue))
      (store 7 65535)))
  (values))

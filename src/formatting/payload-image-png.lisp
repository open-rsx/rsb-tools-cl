;;;; style-image-png.lisp --- Format event payloads as PNG images.
;;;;
;;;; Copyright (C) 2012-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.formatting)

(defclass style-image/png (image-output-mixin)
  ()
  (:documentation
   "This formatting style output image data in PNG format for event
    payloads consisting of image data."))

(service-provider:register-provider/class
 'style :image/png :class 'style-image/png)

(deftype %image-index () '(unsigned-byte 24))

(defmethod format-payload ((data   rst.vision:image)
                           (style  style-image/png)
                           (stream stream)
                           &key
                           (width  (style-width  style))
                           (height (style-height style)))
  (check-type width  dimension-spec/full)
  (check-type height dimension-spec/full)

  (let+ (((&accessors-r/o (in-color  rst.vision:image-color-mode)
                          (in-depth  rst.vision:image-depth)
                          (in-width  rst.vision:image-width)
                          (in-height rst.vision:image-height)
                          (in-pixels rst.vision:image-data))
          data)

         ;; Compute output width, output height and scaling factors
         ;; based on input width, input height and requested width and
         ;; height.
         ((&values out-width scale-x)
          (apply-dimension-spec/integer-factor in-width width))
         ((&values out-height scale-y)
          (apply-dimension-spec/integer-factor in-height height))

         ;; Create the output PNG object.
         (png (make-instance 'zpng:streamed-png
                             :color-type (ecase in-color
                                           (:color-grayscale
                                            :grayscale)
                                           (:color-rgba
                                            :truecolor-alpha)
                                           ((:color-rgb :color-bgr :color-yuv422)
                                            :truecolor))
                             :width      out-width
                             :height     out-height))
         (row (zpng:row-data png)))
    (declare (type %image-index scale-x scale-y
                                in-width out-width out-height)
             (type nibbles:simple-octet-vector in-pixels row)
             (dynamic-extent row))

    (zpng:start-png png stream)
    (fill row 255)

    ;; Transfer and convert pixels. Scaling factors implement
    ;; requested resizing.
    (macrolet
        ((define-decoders ((pixel-format-form depth-form) &body body)
           (once-only (pixel-format-form depth-form)
             (let+ (((&flet+ define-decoder (((pixel-format depth bytes-per-pixel
                                               &optional (units-per-row 'out-width))
                                              &rest body))
                       `((and (eq ,pixel-format ,pixel-format-form)
                              (eq ,depth        ,depth-form))
                         (locally
                             (declare #.rsb:+optimization-fast+unsafe+)
                           (let ((row-fixup (- (* ,bytes-per-pixel in-width)
                                               (* ,bytes-per-pixel scale-x ,units-per-row))))
                             (iter outer
                                   (repeat out-height)
                                   ,@body
                                   (zpng:write-row row png)
                                   (incf from-offset row-fixup)
                                   (unless (= 1 scale-y)
                                     (incf from-offset (* ,bytes-per-pixel
                                                          in-width
                                                          (1- scale-y)))))))))))
               `(cond
                  ,@(mapcar #'define-decoder body)
                  (t
                   (error "~@<Unsupported pixel-format (~S) and ~
                           depth (~S) combination.~@:>"
                          ,pixel-format-form ,depth-form)))))))

      (define-decoders (in-color in-depth)
        ((:color-grayscale :depth-8u 1)
         (generate (the fixnum from-offset) :from 0 :by scale-x)
         (iter (for (the fixnum to-offset) :from 0 :below out-width)
               (in outer (next from-offset))
               (setf (aref row to-offset) (aref in-pixels from-offset))))

        ((:color-grayscale :depth-16u 1)
         (generate (the fixnum from-offset) :from 0 :by (* 2 scale-x))
         (iter (for (the fixnum to-offset) :from 0 :below out-width)
               (in outer (next from-offset))
               (setf (aref row to-offset) (aref in-pixels (1+ from-offset)))))

        ((:color-grayscale :depth-32f 1)
         (generate (the fixnum from-offset) :from 0 :by (* 4 scale-x))
         (iter (for (the fixnum to-offset) :from 0 :below out-width)
               (in outer (next from-offset))
               (setf (aref row to-offset)
                     (truncate
                      (the (single-float 0f0 1f0)
                           (nibbles:ieee-single-ref/le in-pixels from-offset))
                      #.(float 1/255 1f0)))))

        ((:color-rgba :depth-8u 4)
         (generate (the fixnum from-offset) :from 0 :by (* 4 scale-x))
         (iter (for (the fixnum to-offset) :from 0 :below (* 4 out-width) :by 4)
               (in outer (next from-offset))
               (replace row in-pixels
                        :start1 to-offset   :end1 (+ to-offset   4)
                        :start2 from-offset :end2 (+ from-offset 4))))

        ((:color-rgb :depth-8u 3)
         (generate (the fixnum from-offset) :from 0 :by (* 3 scale-x))
         (iter (for (the fixnum to-offset) :from 0 :below (* 3 out-width) :by 3)
               (in outer (next from-offset))
               (replace row in-pixels
                        :start1 to-offset   :end1 (+ to-offset   3)
                        :start2 from-offset :end2 (+ from-offset 3))))

        ((:color-bgr :depth-8u 3)
         (generate (the fixnum from-offset) :from 0 :by (* 3 scale-x))
         (iter (for (the fixnum to-offset) :from 0 :below (* 3 out-width) :by 3)
               (in outer (next from-offset))
               (setf (aref row (+ to-offset 0)) (aref in-pixels (+ from-offset 2))
                     (aref row (+ to-offset 1)) (aref in-pixels (+ from-offset 1))
                     (aref row (+ to-offset 2)) (aref in-pixels (+ from-offset 0)))))

        ((:color-yuv422 :depth-8u 2 (* 2 (ceiling out-width 2)))
         (generate (the fixnum from-offset) :from 0 :by (* 4 scale-x))
         (iter (for (the fixnum to-offset) :from 0 :below (* 3 out-width) :by 6)
               (in outer (next from-offset))
               (%yuv422->rgb in-pixels from-offset row to-offset)))))

    (zpng:finish-png png)

    (force-output stream)
    (values)))

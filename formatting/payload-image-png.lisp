;;;; style-image-png.lisp --- Format event payloads as PNG images.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.formatting)

(defmethod find-style-class ((spec (eql :image/png)))
  (find-class 'style-image/png))

(defclass style-image/png (image-output-mixin)
  ()
  (:documentation
   "This formatting style output image data in PNG format for event
payloads consisting of image data."))

(defmethod format-payload ((data   rst.vision:image)
                           (style  style-image/png)
                           (stream stream)
                           &key
                           (width  (style-width  style))
                           (height (style-height style))
                           &allow-other-keys)
  (check-type width  dimension-spec/full)
  (check-type height dimension-spec/full)

  (let+ (((&accessors-r/o (in-color  rst.vision:image-color-mode)
                          (in-width  rst.vision:image-width)
                          (in-height rst.vision:image-height)
                          (in-pixels rst.vision:image-data)) data)

         ;; Compute output width, output height and scaling factors
         ;; based on input width, input height and requested width and
         ;; height.
         ((&values out-width scale-x)
          (apply-dimension-spec/integer-factor in-width width))
         ((&values out-height scale-y)
          (apply-dimension-spec/integer-factor in-height height))

         ;; Create the output PNG object.
         (png (make-instance 'zpng:pixel-streamed-png
                             :color-type :truecolor-alpha
                             :width      out-width
                             :height     out-height))
         (row (zpng:row-data png)))
    (declare (type positive-fixnum scale-x scale-y out-width out-height))

    (zpng:start-png png stream)
    (fill row 255)

    ;; Transfer and convert pixels. Scaling factors implement
    ;; requested resizing.
    (macrolet
        ((define-decoders (var &body body)
           (let+ (((&flet+ define-decoder ((pixel-format &rest body))
                     `(,pixel-format
                       (iter outer
                             (repeat out-height)
                             ,@body
                             (zpng:write-row row png)
                             (unless (= 1 scale-y)
                               (incf from-offset (* 2 in-width (1- scale-y)))))))))
             `(ecase ,var
                ,@(mapcar #'define-decoder body)))))
      (define-decoders in-color
        (:color-rgb
          (generate (the fixnum from-offset) :from 0 :by (* 3 scale-x))
          (iter (for (the fixnum to-offset) :from 0 :below (* 4 out-width) :by 4)
                (in outer (next from-offset))
                (replace row in-pixels
                         :start1 to-offset   :end1 (+ to-offset 3)
                         :start2 from-offset :end2 (+ from-offset 3))))

        (:color-bgr
          (generate (the fixnum from-offset) :from 0 :by (* 3 scale-x))
          (iter (for (the fixnum to-offset) :from 0 :below (* 4 out-width) :by 4)
                (in outer (next from-offset))
                (setf (aref row (+ to-offset 0)) (aref in-pixels (+ from-offset 2))
                      (aref row (+ to-offset 1)) (aref in-pixels (+ from-offset 1))
                      (aref row (+ to-offset 2)) (aref in-pixels (+ from-offset 0)))))

        (:color-yuv422
          (generate (the fixnum from-offset) :from 0 :by (* 4 scale-x))
          (iter (for (the fixnum to-offset)
                     :from 0 :below (* 4 out-width) :by 8)
                (in outer (next from-offset))
                (%yuv422->rgba in-pixels from-offset row to-offset)))))

    (zpng:finish-png png)

    (force-output stream)))

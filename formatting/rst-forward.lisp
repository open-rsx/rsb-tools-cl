;;;; rst-forward.lisp --- Forward definitions of RST types to avoid dependency.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

;;; rst.vision Types

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (cl:unless (cl:find-package '#:rst.vision)
    (cl:defpackage #:rst.vision
      (:export
       #:image
       #:image-color-mode
       #:image-width
       #:image-height
       #:image-data))))

(cl:in-package #:rst.vision)

(cl:unless (cl:find-class 'image cl:nil)
  (cl:defclass image ()
    ((color-mode :reader image-color-mode)
     (width      :reader image-width)
     (height     :reader image-height)
     (data       :reader image-data))))

;;; rst.audition Types

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (cl:unless (cl:find-package '#:rst.audition)
    (cl:defpackage #:rst.audition
      (:export
       #:sound-chunk
       #:sound-chunk-channels
       #:sound-chunk-rate
       #:sound-chunk-sample-type
       #:sound-chunk-data))))

(cl:in-package #:rst.audition)

(cl:unless (cl:find-class 'sound-chunk cl:nil)
  (cl:defclass sound-chunk ()
    ((channels    :reader sound-chunk-channels)
     (rate        :reader sound-chunk-rate)
     (sample-type :reader sound-chunk-sample-type)
     (data        :reader sound-chunk-data))))

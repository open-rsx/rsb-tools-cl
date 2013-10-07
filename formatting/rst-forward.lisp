;;;; rst-forward.lisp --- Forward definitions of RST types to avoid dependency.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

;;; rst.vision Types

(cl:defpackage :rst.vision
  (:use
   :cl)

  (:export
   :image
   :image-color-mode
   :image-width
   :image-height
   :image-data))

(cl:in-package :rst.vision)

(defclass image ()
  ((color-mode :reader image-color-mode)
   (width      :reader image-width)
   (height     :reader image-height)
   (data       :reader image-data)))

;;; rst.audition Types

(cl:defpackage :rst.audition
  (:use
   :cl)

  (:export
   :sound-chunk
   :sound-chunk-channels
   :sound-chunk-rate
   :sound-chunk-sample-type
   :sound-chunk-data))

(cl:in-package :rst.audition)

(defclass sound-chunk ()
  ((channels    :reader sound-chunk-channels)
   (rate        :reader sound-chunk-rate)
   (sample-type :reader sound-chunk-sample-type)
   (data        :reader sound-chunk-data)))

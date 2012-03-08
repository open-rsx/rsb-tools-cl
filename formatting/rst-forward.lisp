;;; rst-forward.lisp --- Forward definitions of RST types to avoid dependency.
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


;;; rst.vision Types
;;

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
;;

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

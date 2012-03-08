;;; payload-audio.lisp --- Format event payloads as audio streams.
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

(cl:in-package :rsb.formatting)

(defclass style-audio-stream ()
  ()
  (:documentation
   "This class is intended to be used as a superclass for style
classes that produce streams of audio data from event payloads."))

(defmethod format-event ((event  event)
			 (style  style-audio-stream)
			 (target t)
			 &key &allow-other-keys)
  (format-payload (event-data event) style target))

(defmethod find-style-class ((spec (eql :audio-stream/raw)))
  (find-class 'style-audio-stream/raw))

(defclass style-audio-stream/raw (style-audio-stream)
  ()
  (:documentation
   "This style produces a stream of raw audio samples from event
payloads of type rst.audio.SoundChunk."))

(defmethod format-payload ((data   rst.audition:sound-chunk)
			   (style  style-audio-stream/raw)
			   (target t)
			   &key &allow-other-keys)
  (write-sequence (rst.audition:sound-chunk-data data) target))

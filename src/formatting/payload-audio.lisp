;;;; payload-audio.lisp --- Format event payloads as audio streams.
;;;;
;;;; Copyright (C) 2012, 2013, 2014, 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.formatting)

(defclass style-audio-stream (data-consistency-mixin)
  ()
  (:documentation
   "This class is intended to be used as a superclass for style
    classes that produce streams of audio data from event payloads."))

(defmethod format-event ((event  event)
                         (style  style-audio-stream)
                         (target t)
                         &key)
  (format-payload (event-data event) style target))

(defmethod make-descriptor ((style  style-audio-stream)
                            (data   rst.audition:sound-chunk)
                            (target t))
  (list (rst.audition:sound-chunk-channels    data)
        (rst.audition:sound-chunk-rate        data)
        (rst.audition:sound-chunk-sample-type data)))

(defmethod incompatible-descriptors ((style        style-audio-stream)
                                     (descriptor-1 list)
                                     (descriptor-2 list))
  (error "~@<Data format of current sound chunk (~{~A x ~A Hz, ~A~}) ~
          is different from the format of previous chunks (~{~A x ~A ~
          Hz, ~A~}).~@:>"
         descriptor-1 descriptor-2))

(defclass style-audio-stream/raw (style-audio-stream)
  ()
  (:documentation
   "This style produces a stream of raw audio samples (i.e. without a
    header or other format information) from event payloads containing
    audio data."))

(service-provider:register-provider/class
 'style :audio-stream/raw :class 'style-audio-stream/raw)

(defmethod format-payload ((data   rst.audition:sound-chunk)
                           (style  style-audio-stream/raw)
                           (target t)
                           &key)
  (write-sequence (rst.audition:sound-chunk-data data) target))

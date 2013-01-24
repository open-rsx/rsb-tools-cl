;;; payload-wav.lisp --- Format event data as WAV files.
;;
;; Copyright (C) 2012, 2013 Jan Moringen
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

(defmethod find-style-class ((spec (eql :audio-stream/wav)))
  (find-class 'style-audio-stream/wav))

(defclass style-audio-stream/wav (style-audio-stream/raw)
  ()
  (:documentation
   "This style produces an audio stream in WAV format from event
payloads containing audio data."))

(defmethod write-header ((descriptor list)
			 (style      style-audio-stream/wav)
			 (target     t))
  (let+ (((channels sample-rate sample-format) descriptor)
	 (width     (ecase sample-format
		      ((:sample-s8  :sample-u8)  1)
		      ((:sample-s16 :sample-u16) 2)
		      ((:sample-s24 :sample-u24) 3)))
	 (byte-rate (* channels sample-rate width))
	 ;; The non-constant fields have the following meanings:
	 ;; 0 (offset  4 size 4) Size
	 ;; 2 (offset 22 size 2) Number of Channels
	 ;; 3 (offset 24 size 4) Sample Rate
	 ;; 4 (offset 28 size 4) Byte Rate
	 ;; 5 (offset 32 size 2) Block Alignment
	 ;; 6 (offset 34 size 2) Bits per Sample
	 ;; 7 (offset 40 size 4) Sub Chunk Size
	 (wave-header-template
	  (vector #x52 #x49 #x46 #x46    0    0    0    0 #x57 #x41 #x56 #x45
		  #x66 #x6d #x74 #x20 #x10 #x00 #x00 #x00 #x01 #x00    2    2
		     3    3    3    3    4    4    4    4    5    5    6    6
		  #x64 #x61 #x74 #x61    7    7    7    7))
	 ;; Assumed maximum possible size
	 (fake-size #x7fffffff))
    (macrolet
	((set-field (start size value)
	   (once-only (value)
	     `(iter (for (the fixnum i) :below ,size)
		    (setf (aref wave-header-template (+ ,start i))
			  (ldb (byte 8 (* i 8)) ,value))))))

      ;; Regarding "Size" and "Sub chunk size": We use
      ;;
      ;;   Size:           FAKE-SIZE
      ;;   Sub chunk size: (- FAKE-SIZE 44)
      ;;
      ;; where FAKE-SIZE is the assumed maximum size (#0x7fffffff),
      ;; since we cannot know the file size in advance. This seems to
      ;; make most software do the right thing.
      (set-field  4 4 fake-size)          ;; Size
      (set-field 22 2 channels)           ;; Number of channels
      (set-field 24 4 sample-rate)        ;; Sample rate
      (set-field 28 4 byte-rate)          ;; Byte rate
      (set-field 32 2 (* channels width)) ;; Block alignment
      (set-field 34 2 (* width 8))        ;; Bits per sample
      (set-field 40 4 (- fake-size 44)))  ;; Sub chunk size
    (write-sequence wave-header-template target)))

(defmethod (setf descriptor-for-target) :after ((new-value list)
						(style     style-audio-stream/wav)
						(target    t))
  ;; Write WAV header, since we have not done that yet if a descriptor
  ;; for TARGET is installed.
  (write-header new-value style target))

;;; package.lisp --- Package definition for the formatting module.
;;
;; Copyright (C) 2011 Jan Moringen
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

(cl:defpackage :rsb.formatting
  (:use
   :cl
   :alexandria
   :bind
   :iterate

   :rsb)

  ;; Event formatting protocol
  (:export
   :format-event
   :format-payload)

  ;; Formatting style class family
  (:export
   :no-such-style-class
   :find-style-class
   :style-classes)

  ;; Column protocol
  (:export
   :column-name
   :column-width
   :column-produces-output?)

  ;; Column class family
  (:export
   :no-such-column-class
   :find-column-class
   :column-classes)

  ;; `width-mixin' mixin class
  (:export
   :width-mixin

   :column-alignment

   :invoke-width-width-limit
   :with-width-limit)

  ;; `counting-mixin' mixin class
  (:export
   :couting-mixin

   :style-count)

  ;; `header-printing-mixin' mixin class
  (:export
   :header-printing-mixin

   :style-header-frequency)

  ;; `columns-mixin' mixin class
  (:export
   :columns-mixin

   :style-columns
   :style-separator

   :make-column)

  ;; `periodic-printing-mixin' mixin class
  (:export
   :periodic-printing-mixin

   :style-print-interval)

  ;; `delegating-mixin' mixin class
  (:export
   :delegating-mixin

   :style-sub-styles)

  ;; Formatting functions
  (:export
   :format-octet-vector
   :format-string)

  (:documentation
   "This package contains formatting functions for RSB events and
event payloads."))

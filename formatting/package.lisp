;;; package.lisp --- Package definition for the formatting module.
;;
;; Copyright (C) 2011, 2012 Jan Moringen
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
   :let-plus
   :iterate

   :rsb)

  ;; Types
  (:export
   :rule-spec
   :separator-spec

   :template-designator)

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

  ;; `name-mixin' mixin class
  (:export
   :name-mixin)

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

  ;; `separator-mixin' mixin class
  (:export
   :separator-mixin

   :style-separator)

  ;; `style-meta-data' style class
  (:export
   :style-meta-data

   :style-routing-info?
   :style-timestamps?
   :style-user-items?
   :style-causes?)

  ;; `style-detailed' style class
  (:export
   :style-detailed)

  ;; `style-compact[+]' style classes
  (:export
   :style-compact
   :style-compact+)

  ;; `style-statistics' style class
  (:export
   :style-statistics)

  ;; Default variable names provided by programmable styles
  (:export
   :sequence-number
   :id
   :scope
   :origin
   :data
   :create  :create-unix  :create-unix-nsec
   :send    :send-unix    :send-unix-nsec
   :receive :receive-unix :receive-unix-nsec
   :deliver :deliver-unix :deliver-unix-nsec
   :causes/event-id
   :causes/uuid
   :skip-event)

  ;; `style-programmable' style class
  (:export
   :style-programmable

   :style-code
   :style-bindings

   :compile-code)

  ;; `style-programmable/script' style class
  (:export
   :style-programmable/script

   :style-script)

  ;; `style-programmable/template' style class
  (:export
   :style-programmable/template

   :style-template)

  ;; Formatting functions
  (:export
   :format-octet-vector
   :format-string)

  ;; Stream-related functions
  (:export
   :stream-line-width

   :with-print-limits
   :invoke-with-print-limits)

  ;;Help text generation
  (:export
   :make-style-help-string)

  (:documentation
   "This package contains formatting functions for RSB events and
event payloads."))

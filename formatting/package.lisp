;;;; package.lisp --- Package definition for the formatting module.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb.formatting
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:iterate
   #:more-conditions

   #:nibbles

   #:rsb)

  ;; Types
  (:export
   #:timestamp/unix/nsec
   #:time-spec
   #:bounds-spec
   #:bounds

   #:rule-spec
   #:separator-spec

   #:print-interval

   #:dimension-spec/short
   #:dimension-spec/full
   #:dimension-spec

   #:template-designator
   #:script-designator)

  ;; Conditions
  (:export
   #:format-code-error ; condition class and function
   #:format-code-error-code

   #:simple-format-code-error

   #:format-code-read-error)

  ;; Variables
  (:export
   #:*textual-output-can-use-utf-8?*)

  ;; Event formatting protocol
  (:export
   #:format-event
   #:format-payload)

  ;; Formatting style class family
  (:export
   #:no-such-style-class
   #:find-style-class
   #:style-classes

   #:make-style)

  ;; Collecting protocol
  (:export
   #:collects?)

  ;; Delegation protocol
  (:export
   #:style-sub-styles
   #:sub-style-for

   #:delegate

   #:make-sub-style-entry)

  ;; Sub-style sorting protocol
  (:export
   #:style-sub-styles/sorted)

  ;; Sub-style pruning protocol
  (:export
   #:style-prune-predicate ; also setf
   #:prune-sub-styles)

  ;; Data consistency protocol
  (:export
   #:descriptor-for-target
   #:make-descriptor
   #:compatible-descriptors?
   #:incompatible-descriptors)

  ;; Temporal bounds protocol
  (:export
   #:lower-bound
   #:upper-bound
   #:bounds
   #:bounds/expanded
   #:range/expanded)

  ;; Column protocol
  (:export
   #:value<
   #:column<

   #:column-name
   #:column-width
   #:column-produces-output?)

  ;; Column class family
  (:export
   #:no-such-column-class
   #:find-column-class
   #:column-classes

   #:make-column)

  ;; Header protocol
  (:export
   #:format-header)

  ;; Width computation
  (:export
   #:optimize-widths)

  ;; `width-mixin' mixin class
  (:export
   #:width-mixin

   #:column-alignment

   #:call-width-width-limit
   #:with-width-limit)

  ;; `counting-mixin' mixin class
  (:export
   #:couting-mixin

   #:style-count)

  ;; `activity-tacking-mixin' mixin class
  (:export
   #:activity-tracking-mixin

   #:style-last-activity)

  ;; `header-printing-mixin' mixin class
  (:export
   #:header-printing-mixin

   #:style-header-frequency)

  ;; `basic-column' class
  (:export
   #:basic-column)

  ;; `column-constant' class
  (:export
   #:oclumn-constant

   #:column-value
   #:column-formatter)

  ;; `columns-mixin' mixin class
  (:export
   #:columns-mixin

   #:style-columns
   #:style-separator)

  ;; `periodic-printing-mixin' mixin class
  (:export
   #:periodic-printing-mixin

   #:style-print-interval)

  ;; `delegating-mixin' mixin class
  (:export
   #:delegating-mixin)

  ;; `sub-style-grouping-mixin' mixin class
  (:export
   #:sub-style-grouping-mixin

   #:style-key
   #:style-test)

  ;; `sub-style-sorting-mixin'
  (:export
   #:sub-style-sorting-mixin

   #:style-sort-predicate
   #:style-sort-key)

  ;; `sub-style-pruning-mixin' and
  ;; `activity-based-sub-style-pruning-mixin'
  (:export
   #:sub-style-pruning-mixin
   #:activity-based-sub-style-pruning-mixin)

  ;; `separator-mixin' mixin class
  (:export
   #:separator-mixin

   #:style-separator)

  ;; `image-output-mixin' mixin class
  (:export
   #:image-output-mixin

   #:style-width
   #:style-height

   #:normalize-dimension-spec
   #:apply-dimension-spec)

  ;; `data-consistency-mixin' mixin class
  (:export
   #:data-consistency-mixin)

  ;; `timestamp-mixin' mixin class
  (:export
   #:timestamp-mixin

   #:style-timestamp)

  ;; `temporal-bounds-mixin' mixin class
  (:export
   #:temporal-bounds-mixin)

  ;; `style-meta-data' style class
  (:export
   #:style-meta-data

   #:style-routing-info?
   #:style-timestamps?
   #:style-user-items?
   #:style-causes?)

  ;; `style-detailed' style class
  (:export
   #:style-detailed)

  ;; `style-compact' style classes
  (:export
   #:style-compact/80
   #:style-compact/128
   #:style-compact/180
   #:style-compact)

  ;; `style-statistics' style classes
  (:export
   #:style-statistics/80
   #:style-statistics/128
   #:style-statistics/180
   #:style-statistics/220
   #:style-statistics)

  ;; Default variable names provided by programmable styles
  (:export
   #:sequence-number
   #:id
   #:scope
   #:origin
   #:data
   #:create  #:create-unix  #:create-unix-nsec
   #:send    #:send-unix    #:send-unix-nsec
   #:receive #:receive-unix #:receive-unix-nsec
   #:deliver #:deliver-unix #:deliver-unix-nsec
   #:causes/event-id
   #:causes/uuid
   #:skip-event)

  ;; `style-programmable' style class
  (:export
   #:style-programmable

   #:style-code
   #:style-bindings

   #:compile-code)

  ;; `style-programmable/script' style class
  (:export
   #:style-programmable/script

   #:style-script)

  ;; `style-programmable/template' style class
  (:export
   #:style-programmable/template

   #:style-template)

  ;; `style-image/png' style class
  (:export
   #:style-image/png)

  ;; `style-audio-stream' style class
  (:export
   #:style-audio-stream)

  ;; `style-audio-stream/raw' style class
  (:export
   #:style-audio-stream/raw)

  ;; `style-audio-stream/wav' style class
  (:export
   #:style-audio-stream/wav)

  ;; Formatting functions
  (:export
   #:format-octet-vector
   #:format-string)

  ;; Stream-related functions
  (:export
   #:stream-line-width

   #:with-print-limits
   #:call-with-print-limits)

  ;; Help text generation
  (:export
   #:make-style-help-string)

  (:documentation
   "This package contains formatting functions for RSB events and
    event payloads."))

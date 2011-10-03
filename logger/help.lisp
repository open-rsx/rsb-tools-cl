;;; help.lisp --- Help text generation for the logger program.
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

(in-package :rsb.tools.logger)

(defun make-help-string ()
  "Return a help that explains the commandline option interface."
  (with-output-to-string (stream)
    (format stream "Show events exchanged on the RSB channel ~
designated by URI. Events can be filtered and displayed in several ~
ways which can be controlled using the --filter and --style options.

URI designates the channel for which events should be received and ~
logged and the transport that should be used to attach to channel. A ~
URI of the form

  ")
    (print-uri-help stream)))

(defun make-filter-help-string ()
  "Return a help string that explains how to specify filters and lists
the available filters."
  (with-output-to-string (stream)
    (format stream "Specify a filter that received events have to ~
match in order to be processed rather than discarded. This option can ~
be supplied multiple times in which case events have to match all ~
specified filters. Each SPEC has to be of one of the forms

  KIND | KIND SINGLE-VALUE | KIND KEY1 VALUE1 KEY2 VALUE2 ...

where keys and values depend on KIND and may be mandatory in some ~
cases. Examples (note that the single quotes have to be included only ~
when used within a shell):

  --filter 'origin \"EAEE2B00-AF4B-11E0-8930-001AA0342D7D\"'
  --filter 'regex \".*foo[0-9]+\"'
  --filter 'regex :regex \".*foo[0-9]+\"' (equivalent)
  -f 'xpath :xpath \"node()/@foo\" :fallback-policy :do-not-match'

The following filters are currently available (paragraph headings ~
correspond to respective KIND):

")
    (print-filter-help stream)))

(defun make-style-help-string (&key
			       (show :default))
  "Return a help string that explains how to specify a formatting
style and its parameters."
  (with-output-to-string (stream)
    (format stream "Specify a formatting style that should be used to ~
print events. SPEC has to be of the form

  KIND KEY1 VALUE1 KEY2 VALUE2 ...

where keys and values are optional and depend on KIND. Examples (note ~
that the single quotes have to be included only when used within a ~
shell):

  --style detailed
  -s compact
  --style 'compact :separator \"|\"'
  --style 'columns :columns (:now (:scope :width 12) :id :newline)' ~
\(see below for an explanation of the :columns argument\)

The following formatting styles are currently available:

")
    (print-classes-help-string
     (style-classes) stream
     :initarg-blacklist '(:stream :quantities :count :sub-styles))

    (when (or (eq show t)
	      (member :columns (ensure-list show)))
      (format stream "

In column-based formatting styles, columns can be selected and ~
configured using the :columns argument and a syntax of the form

  :columns (COLSPEC1 COLSPEC2 ...)

where

  COLSPEC ::= KIND | (KIND KEY1 VALUE1 KEY2 VALUE2 ...)

The following columns are available:

")
      (print-classes-help-string
       (column-classes) stream))

    (when (or (eq show t)
	      (member :quantities (ensure-list show)))
      (format stream "

In the statistics style, statistical quantities are used in ~
columns. These columns can be configured using the :columns argument ~
and a syntax of the form

  :columns (COLSPEC1 COLSPEC2 ...)

where

  COLSPEC ::= (:quantity :quantity QUANTITY VALUE1 KEY2 VALUE2 ...)
              | (:quantity :quantity (QUANTITY KEY1 VALUE1 KEY2 VALUE2 ...) KEY1 VALUE1 KEY2 VALUE2 ...)

The following quantities are available:

")
      (print-classes-help-string
       (rsb.stats:quantity-classes) stream
       :initarg-blacklist '(:extractor :reduce-by :start-time :values)))))

(defun make-examples-string (&key
			       (program-name "logger"))
  "Make and return a string containing usage examples of the program."
  (format nil "~A

  Use all enabled transports with their respective default ~
configuration to access the bus. Receive and display all events ~
exchanged on the entire bus (since the channel designated by the root ~
scope, \"/\", is implicitly used).

~:*~A spread://localhost:4811

  Use the Spread daemon listening on port 4811 on localhost to connect ~
to the bus. Since no scope is specified, receive and print all events ~
exchanged on the entire bus.

~:*~A -f 'regex :regex \"^mypattern\" :fallback-policy :do-not-match' ~
--style detailed spread:/my/channel

  Use the default configuration of the Spread transport to connect to ~
the bus. Receive events on the channel designated by ~
\"/my/channel\" (and sub-channels) the payloads of which match the ~
regular expression \"^mypattern\". Display matching event using the ~
\"detailed\" display style.

~:*~A -f \"$(cat my-complex-filter)\" -s \"$(cat my-complex-style)\" ~
/some/scope

  Use the contents of the files \"my-complex-filter\" and ~
\"my-complex-style\" to specify a filter expression and an event ~
formatting style respectively. Note that the syntactic details depend ~
on the used shell and that the idiom is not specific to ~:*~A. As an ~
example, a file with the following content would mimic the \"compact\" ~
style when used as argument to the --style (-s) option:

  columns
  :columns (:now
            :origin :sequence-number :method :id :scope :data :data-size
            :newline)
"
	  program-name))
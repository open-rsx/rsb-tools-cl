;;; style-meta-data.lisp --- Unit tests for the meta-data formatting style.
;;
;; Copyright (C) 2011, 2012, 2013 Jan Moringen
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

(cl:in-package :rsb.formatting.test)

(deftestsuite style-meta-data-root (formatting-root)
  ()
  (:documentation
   "Unit tests for the `style-meta-data' formatting style class."))

(addtest (style-meta-data-root
          :documentation
	  "Test some simple cases of formatting events using methods
on `format-event' for `style-meta-data'.")
  smoke

  (ensure-style-cases (style-meta-data)
    '(()
      ()
      "")

    `(()
      (,(make-event "/foo" "bar"))
      ,(format nil "~80,,,VA
Event
  Scope          : /foo/
  Id             : N/A
  Sequence-Number: N/A
  Origin         : N/A
  Method         : N/A
Timestamps
  Create : .*
  Send   : N/A
  Receive: N/A
  Deliver: N/A
"
	       (if *textual-output-can-use-utf-8?* #\─ #\-) ""))

    `(()
      (,(make-event "/foo/bar/baz" 1 :fez "whoop"))
      ,(format nil "~80,,,VA
Event
  Scope          : /foo/bar/baz/
  Id             : N/A
  Sequence-Number: N/A
  Origin         : N/A
  Method         : N/A
Timestamps
  Create : .*
  Send   : N/A
  Receive: N/A
  Deliver: N/A
Meta-Data
  Fez: whoop
"
	       (if *textual-output-can-use-utf-8?* #\─ #\-) ""))))

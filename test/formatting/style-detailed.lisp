;;;; style-detailed.lisp --- Unit tests for the detailed formatting style.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.formatting.test)

(deftestsuite style-detailed-root (formatting-root)
  ()
  (:documentation
   "Unit tests for the `style-detailed' formatting style class."))

(addtest (style-detailed-root
          :documentation
          "Test some simple cases of formatting events using methods
           on `format-event' for `style-detailed'.")
  smoke

  (ensure-style-cases (style-detailed)
    '(()
      ()
      "")

    `(()
      (,(make-event "/foo" "bar"))
      ,(format nil "~80,,,VA
Event
  Scope           /foo/
  Id              <none>
  Sequence number <none>
  Origin          <none>
  Method          <none>
Timestamps
  create  .*
  send    <none>
  receive <none>
  deliver <none>
Payload: STRING, 3 characters
  \"bar\"
"
               (if *textual-output-can-use-utf-8?* #\─ #\-) ""))

    `(()
      (,(make-event "/foo/bar/baz" 1 :fez "whoop"))
      ,(format nil "~80,,,VA
Event
  Scope           /foo/bar/baz/
  Id              <none>
  Sequence number <none>
  Origin          <none>
  Method          <none>
Timestamps
  create  .*
  send    <none>
  receive <none>
  deliver <none>
Meta-Data
  FEZ \"whoop\"
Payload: .*
  1
"
               (if *textual-output-can-use-utf-8?* #\─ #\-) ""))
    `(()
      (,(make-event "/foo/bar/baz" (nibbles:octet-vector 1 2 255)))
      ,(format nil "~80,,,VA
Event
  Scope           /foo/bar/baz/
  Id              <none>
  Sequence number <none>
  Origin          <none>
  Method          <none>
Timestamps
  create  .*
  send    <none>
  receive <none>
  deliver <none>
Payload: .*, 3 octets
  0 01 02 FF                                              ... *~@
"
               (if *textual-output-can-use-utf-8?* #\─ #\-) ""))))

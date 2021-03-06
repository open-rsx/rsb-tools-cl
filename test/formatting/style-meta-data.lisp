;;;; style-meta-data.lisp --- Unit tests for the meta-data formatting style.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.formatting.test)

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
      ,(format nil "
~80,,,VA
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
  deliver <none>"
               (if *output-unicode?* #\─ #\-) ""))

    `(()
      (,(make-event "/foo/bar/baz" 1 :fez "whoop"))
      ,(format nil "
~80,,,VA
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
  FEZ \"whoop\""
               (if *output-unicode?* #\─ #\-) ""))

    `(()
      (,(let ((event (make-event "/foo/bar/baz" 1)))
          (setf (timestamp event :foo) (local-time:now))
          event))
      ,(format nil "
~80,,,VA
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
  \\*foo    .*"
               (if *output-unicode?* #\─ #\-) ""))))

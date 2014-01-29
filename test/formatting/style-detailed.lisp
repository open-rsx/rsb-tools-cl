;;;; style-detailed.lisp --- Unit tests for the detailed formatting style.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
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
Payload \\(SB-KERNEL::SIMPLE-CHARACTER-STRING\\)
  \"bar\"
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
Payload \\(.*\\)
  1
"
               (if *textual-output-can-use-utf-8?* #\─ #\-) ""))))

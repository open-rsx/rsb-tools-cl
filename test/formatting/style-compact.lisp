;;;; style-compact.lisp --- Unit tests for the compact formatting style.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.formatting.test)

(deftestsuite style-compact-root (formatting-root)
  ()
  (:documentation
   "Unit tests for the `style-compact' formatting style class."))

(addtest (style-compact-root
          :documentation
          "Test some simple cases of formatting events using methods
           on `format-event' for `style-compact'.")
  smoke

  (let ((*print-right-margin* 80))
    (ensure-style-cases (event-style-compact)
      '(()
        ()
        "")

      `((:header-frequency nil)
        (,(make-event "/foo" "bar"))
        ".*│EVENTID…│<nomethod>│/foo/    │\"bar\"     │WIRE-SC…│  3 B|ORIGIN?.
")

      `((:header-frequency nil)
        (,(make-event "/foo" "bar") ,(make-event "/fez" "whoop"))
        ".*│EVENTID…│<nomethod>│/foo/    │\"bar\"     │WIRE-SC…│  3 B|ORIGIN?.
.*│EVENTID…│<nomethod>│/fez/    │\"whoop\"   │WIRE-SC…│  5 B|ORIGIN?.
")))

  (let ((*print-right-margin* 128))
    (ensure-style-cases (event-style-compact)
      '(()
        ()
        "")

      `((:header-frequency nil)
        (,(make-event "/foo" "bar"))
        ;; 15 8 10 21 21 20 9 8 8
        ".*│EVENTID…│<nomethod>│/foo/                          │\"bar\"                  │WIRE-SCHEMA\\?│  3 B│ORIGIN\\? │     NIL
")
      `((:header-frequency nil)
        (,(make-event "/foo" "bar") ,(make-event "/fez" "whoop"))
        ".*│EVENTID…│<nomethod>│/foo/                          │\"bar\"                  │WIRE-SCHEMA\\?│  3 B│ORIGIN\\? │     NIL
.*│EVENTID…│<nomethod>│/fez/                          │\"whoop\"                │WIRE-SCHEMA\\?│  5 B│ORIGIN\\? │     NIL
"))))

;; Local Variables:
;; coding: utf-8
;; End:

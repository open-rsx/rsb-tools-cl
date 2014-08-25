;;;; style-compact.lisp --- Unit tests for the compact formatting style.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
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
        ".*│EVENTID…│<nomethod>│/foo/   │\"bar\"   │ORIGIN\\? │     NIL│WIRE-SC…
")

      `((:header-frequency nil)
        (,(make-event "/foo" "bar") ,(make-event "/fez" "whoop"))
        ".*│EVENTID…│<nomethod>│/foo/   │\"bar\"   │ORIGIN\\? │     NIL│WIRE-SC…
.*│EVENTID…│<nomethod>│/fez/   │\"whoop\" │ORIGIN\\? │     NIL│WIRE-SC…
")))

  (let ((*print-right-margin* 128))
    (ensure-style-cases (event-style-compact)
      '(()
        ()
        "")

      `((:header-frequency nil)
        (,(make-event "/foo" "bar"))
        ".*│EVENTID…│<nomethod>│/foo/               │\"bar\"                   │ORIGIN\\? │     NIL│WIRE-SCHEMA\\?      │        3
")
      `((:header-frequency nil)
        (,(make-event "/foo" "bar") ,(make-event "/fez" "whoop"))
        ".*│EVENTID…│<nomethod>│/foo/               │\"bar\"                   │ORIGIN\\? │     NIL│WIRE-SCHEMA\\?      │        3
.*│EVENTID…│<nomethod>│/fez/               │\"whoop\"                 │ORIGIN\\? │     NIL│WIRE-SCHEMA\\?      │        5
"))))

;; Local Variables:
;; coding: utf-8
;; End:

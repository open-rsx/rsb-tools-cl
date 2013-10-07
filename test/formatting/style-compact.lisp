;;;; style-compact.lisp --- Unit tests for the compact family of styles.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsb.formatting.test)

(deftestsuite style-compact/80-root (formatting-root)
  ()
  (:documentation
   "Unit tests for the `style-compact/80' formatting style class."))

(addtest (style-compact/80-root
          :documentation
          "Test some simple cases of formatting events using methods
on `format-event' for `style-compact/80'.")
  smoke

  (ensure-style-cases (style-compact/80)
    '(()
      ()
      "")

    `((:header-frequency nil)
      (,(make-event "/foo" "bar"))
      ".*│ORIGIN\\? │/foo/                 │\"bar\"                 │        3
")

    `((:header-frequency nil)
      (,(make-event "/foo" "bar") ,(make-event "/fez" "whoop"))
      ".*│ORIGIN\\? │/foo/                 │\"bar\"                 │        3
.*│ORIGIN\\? │/fez/                 │\"whoop\"               │        5
")))

(deftestsuite style-compact/128-root (formatting-root)
  ()
  (:documentation
   "Unit tests for the `style-compact/128' formatting style class."))

(addtest (style-compact/128-root
          :documentation
          "Test some simple cases of formatting events using methods
on `format-event' for `style-compact/128'.")
  smoke

  (ensure-style-cases (style-compact/128)
    '(()
      ()
      "")

    `((:header-frequency nil)
      (,(make-event "/foo" "bar"))
      ".*│ORIGIN\\? │     NIL│EVENTID…│<nomethod>│/foo/                        │\"bar\"                             │        3
")

    `((:header-frequency nil)
      (,(make-event "/foo" "bar") ,(make-event "/fez" "whoop"))
      ".*│ORIGIN\\? │     NIL│EVENTID…│<nomethod>│/foo/                        │\"bar\"                             │        3
.*│ORIGIN\\? │     NIL│EVENTID…│<nomethod>│/fez/                        │\"whoop\"                           │        5
")))

;; Local Variables:
;; coding: utf-8
;; End:

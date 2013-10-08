;;;; width-mixin.lisp --- Unit tests for the width-mixin class.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsb.formatting.test)

(deftestsuite width-mixin-root (formatting-root)
  ()
  (:documentation
   "Test suite for the `width-mixin' mixin class."))

(addtest (width-mixin-root
          :documentation
	  "Test method on `format-event' for `width-mixin'.")
  format-event

  (ensure-style-cases (mock-column)
    '(;; No data => no output
      ()
      ()
      "")

    ;; short data => aligned padding
    '((:width 8 :alignment :right)
      ("foo" "bar")
      "     foo     bar")

    ;; short data => aligned padding
    '((:width 8 :alignment :left)
      ("foo" "bar")
      "foo     bar     ")

    ;; long data => aligned truncation
    '((:width 2 :alignment :right)
      ("foo" "bar")
      "…o…r")

    ;; long data => aligned truncation
    '((:width 2 :alignment :left)
      ("foo" "bar")
      "f…b…")))

(addtest (width-mixin-root
          :documentation
	  "Test method on `format-header' for `width-mixin'.")
  format-header

  (ensure-style-cases (mock-column :formatter :format-header)
    '(;; No data => no output
      ()
      ()
      "")

    ;; short data => aligned padding
    '((:width 8 :alignment :right)
      ("foo" "bar")
      "My mock…My mock…")

    ;; short data => aligned padding
    '((:width 8 :alignment :left)
      ("foo" "bar")
      "My mock…My mock…")

    ;; long data => aligned truncation
    '((:width 2 :alignment :right)
      ("foo" "bar")
      "M…M…")

    ;; long data => aligned truncation
    '((:width 2 :alignment :left)
      ("foo" "bar")
      "M…M…")))

;; Local Variables:
;; coding: utf-8
;; End:

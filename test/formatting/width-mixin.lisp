;;; width-mixin.lisp --- Unit tests for the width-mixin class.
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

;; Local Variables:
;; coding: utf-8
;; End:

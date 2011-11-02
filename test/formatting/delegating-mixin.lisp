;;; delegating-mixin.lisp --- Tests for the delegating-mixin mixin class.
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

(in-package :rsb.formatting.test)

(deftestsuite delegating-mixin-root (formatting-root)
  ()
  (:documentation
   "Test suite for the `delegating-mixin' mixin class."))

(addtest (delegating-mixin-root
          :documentation
	  "Test method on `format-event' for `delegating-mixin' mixin
class.")
  format-event

  (ensure-style-cases (delegating-mixin)
    ;; No data => no output
    '(() () "")

    ;; Unconditionally dispatch to a single sub-style
    `((:sub-styles ((,(constantly t) . ,(make-instance 'mock-column
						       :width 8))))
      ("foo" "bar")
      "     foo     bar")

    ;; No matching sub-style is not an error
    `((:sub-styles ((,#'stringp . ,(make-instance 'mock-column
						  :width 4))))
      ("foo" 5 t)
      " foo")

    ;; Two sub-styles
    `((:sub-styles ((,#'oddp  . ,(make-instance 'mock-column
						:width 8 :alignment :left))
		    (,#'evenp . ,(make-instance 'mock-column
						:width 8 :alignment :right))))
      (1 2 3 4)
      "1              23              4")))

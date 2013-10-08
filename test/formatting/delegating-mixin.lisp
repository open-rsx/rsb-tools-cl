;;;; delegating-mixin.lisp --- Tests for the delegating-mixin mixin class.
;;;;
;;;; Copyright (C) 2011, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsb.formatting.test)

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

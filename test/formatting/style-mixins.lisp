;;;; delegating-mixin.lisp --- Tests for the delegating-mixin mixin class.
;;;;
;;;; Copyright (C) 2011, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.formatting.test)

;;; `delegating-mixin'

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

;;; `activity-based-sub-style-pruning-mixin'

(deftestsuite activity-based-sub-style-pruning-mixin-root (formatting-root)
  ()
  (:documentation
   "Unit tests for the `activity-based-sub-style-pruning-mixin'
    class."))

(addtest (activity-based-sub-style-pruning-mixin-root
          :documentation
          "Test constructing `activity-based-sub-style-pruning-mixin'
           instances")
  construct

  (ensure-cases (initargs expected)
      `(;; Some invalid cases
        ((:prune-after 5 :prune-predicate ,#'identity) incompatible-initargs)

        ;; These should be ok
        (()                                            t)
        ((:prune-after 5)                              t)
        ((:prune-predicate ,#'identity)                t)
        ((:prune-predicate nil)                        t))

    (let+ (((&flet do-it ()
              (apply #'make-instance 'activity-based-sub-style-pruning-mixin
                     initargs))))
      (case expected
        (incompatible-initargs
         (ensure-condition 'incompatible-initargs (do-it)))
        (t
         (do-it))))))

;;; `temporal-bounds-mixin'

(deftestsuite temporal-bounds-mixin-root (formatting-root)
  ()
  (:documentation
   "Unit tests for the `temporal-bounds-mixin' class."))

(addtest (temporal-bounds-mixin-root
          :documentation
          "Test constructing `temporal-bounds-mixin' instances.")
  construct

  (ensure-cases (initargs expected-bounds
                 &optional now expected-bounds/expanded)
      '(;; Some invalid cases.
        ((:lower-bound "now")                        type-error)
        ((:lower-bound :now :upper-bound :now)       type-error)
        ((:lower-bound (+ :now 2) :upper-bound :now) type-error)

        ;; These should be ok
        (()                                          ((- :now 20) :now) 0 (-20000000000 0)))

    (let+ (((&flet do-it ()
              (apply #'make-instance 'temporal-bounds-mixin initargs))))
      (case expected-bounds
        (type-error
         (ensure-condition 'type-error (do-it)))
        (t
         (let ((thing (do-it)))
           (ensure-same expected-bounds          (bounds thing)
                        :test #'equal)
           (ensure-same expected-bounds/expanded (bounds/expanded thing now)
                        :test #'equal)))))))

;;;; dynamic-width.lisp --- Unit test for the width computation code.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.formatting.test)

(deftestsuite dynamic-width-root (formatting-root)
  ()
  (:documentation
   "Unit tests for the `optimize-widths' function."))

(addtest (dynamic-width-root
          :documentation
          "Smoke test for the `optimize-widths' function.")
  optimize-widths/smoke

  (ensure-cases ((width specs priorities &optional separator) expected)
      '(;; Some illegal cases.
        ((0  (1)                          ())    error)
        ((0  ()                           (1))   error)

        ;; These are OK.
        ((0  ()                           ())    :empty)
        ((50 ()                           ())    t)

        ((0  ((:range 5 10))              (3))   :empty)
        ((50 ((:range 5 10))              (3))   t)
        ((9  ((:range 5 10))              (3))   :full)

        ((0  ((:range 5 10) (:range 3 6)) (3 5)) :empty)
        ((50 ((:range 5 10) (:range 3 6)) (3 5)) t)
        ((12 ((:range 5 10) (:range 3 6)) (3 5)) :full)

        ((0  (5 ((:range 3 4) 7))         (3 5)) :empty)
        ((50 (5 ((:range 3 4) 7))         (3 5)) t)
        ((9  (5 ((:range 3 4) 7))         (3 5)) :full)
        ((12 (5 ((:range 3 4) 7))         (3 5)) :full))

    (let+ (((&flet do-it ()
              (let+ (((&values result score)
                      (apply #'optimize-widths
                             width specs priorities
                             (when separator
                               `(:separator-length ,(length separator))))))
                (values result (reduce #'+ result) score)))))
      (ecase expected
        (error
         (ensure-condition error (do-it)))
        ((:empty :full t)
         (let+ (((&values result sum score) (do-it)))
           (ensure-same (length result) (length specs) :test #'=)
           (ecase expected
             (:empty
              (ensure-same sum 0 :test #'=)
              (ensure-same score 0 :test #'=))
             (:full
              (ensure-same sum width :test #'=)
              (ensure (plusp score)))
             ((t)
              (ensure (<= sum width))
              (ensure (typep score 'non-negative-integer))))))))))

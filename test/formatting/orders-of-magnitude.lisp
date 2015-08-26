;;;; orders-of-magnitude.lisp --- Unit test for orders of magnitude functions.
;;;;
;;;; Copyright (C) 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.formatting.test)

(deftestsuite orders-of-magnitude-root (formatting-root)
  ()
  (:documentation
   "Unit tests for the \"orders of magnitude\" functions."))

(macrolet
    ((define-print-human-readable-test ((test-name function-name) &rest cases)
       (let ((documentation (format nil "Smoke test for the `~(~A~)' function."
                                    function-name)))
         `(addtest (orders-of-magnitude-root :documentation ,documentation)
            ,test-name

            (ensure-cases (number expected-1 expected-2 expected-3 expected-4)
                '(,@cases)
              (let+ (((&flet do-it (colon? at? expected)
                        (let ((result (with-output-to-string (stream)
                                        (,function-name
                                         stream number colon? at?))))
                          (ensure-same result expected :test #'string=)))))
                (do-it nil nil expected-1)
                (do-it nil t   expected-2)
                (do-it t   nil expected-3)
                (do-it t   t   expected-4)))))))

  (define-print-human-readable-test
      (print-human-readable-order-of-magnitude/smoke
       print-human-readable-order-of-magnitude)
    ;; Not a suitable input
    ("foo"            " foo" " foo"      "  foo" "  foo")
    ;; Special cases
    (0                "  0 " "  0 "      "   0 " "   0 ")
    (0.0              "  0 " "  0 "      "   0 " "   0 ")
    (-0.0             "  0 " "  0 "      "   0 " "   0 ")
    (1                "  1 " "  1 "      "+  1 " "+  1 ")
    (-1               "  1 " "  1 "      "-  1 " "-  1 ")
    ;; Rounding
    (-0.6             "600m" "600 milli" "-600m" "-600 milli")
    (-0.5             "500m" "500 milli" "-500m" "-500 milli")
    (-0.4             "400m" "400 milli" "-400m" "-400 milli")
    ( 0.4             "400m" "400 milli" "+400m" "+400 milli")
    ( 0.5             "500m" "500 milli" "+500m" "+500 milli")
    ( 0.6             "600m" "600 milli" "+600m" "+600 milli")
    ;; Corner cases
    (9                "  9 " "  9 "      "+  9 " "+  9 ")
    (-9               "  9 " "  9 "      "-  9 " "-  9 ")
    (9.001            "9.0 " "9.0 "      "+9.0 " "+9.0 ")
    (10               " 10 " " 10 "      "+ 10 " "+ 10 ")
    (999              "999 " "999 "      "+999 " "+999 ")
    (1000             "  1K" "  1 kilo"  "+  1K" "+  1 kilo")
    (10000000/1001299 " 10 " " 10 "      "+ 10 " "+ 10 "))

  (define-print-human-readable-test
      (print-human-readable-count/smoke print-human-readable-count)
    ;; Not a suitable input
    ("foo"            " foo"  " foo"    "  foo"  "  foo")
    ;; Special cases
    (0                "  0 "  "  0 "    "   0 "  "   0 ")
    (0.0              "  0 "  "  0 "    "   0 "  "   0 ")
    (-0.0             "  0 "  "  0 "    "   0 "  "   0 ")
    (1                "  1 "  "  1 "    "+  1 "  "+  1 ")
    (-1               "  1 "  "  1 "    "-  1 "  "-  1 ")
    ;; Rounding
    (-0.6             "0.6 "  "0.6 "    "-0.6 "  "-0.6 ")
    (-0.5             "0.5 "  "0.5 "    "-0.5 "  "-0.5 ")
    (-0.4             "0.4 "  "0.4 "    "-0.4 "  "-0.4 ")
    ( 0.4             "0.4 "  "0.4 "    "+0.4 "  "+0.4 ")
    ( 0.5             "0.5 "  "0.5 "    "+0.5 "  "+0.5 ")
    ( 0.6             "0.6 "  "0.6 "    "+0.6 "  "+0.6 ")
    ;; Corner cases
    (9                "  9 "  "  9 "    "+  9 "  "+  9 ")
    (-9               "  9 "  "  9 "    "-  9 "  "-  9 ")
    (9.001            "9.0 "  "9.0 "    "+9.0 "  "+9.0 ")
    (10               " 10 "  " 10 "    "+ 10 "  "+ 10 ")
    (999              "999 "  "999 "    "+999 "  "+999 ")
    (1000             "  1K" "  1 kilo" "+  1K" "+  1 kilo")
    (10000000/1001299 " 10 "  " 10 "    "+ 10 "  "+ 10 "))

  (define-print-human-readable-test
      (print-human-readable-size/smoke print-human-readable-size)
    ;; Not a suitable input
    ("foo"            "  foo" "     foo"     "   foo" "      foo")
    ;; Special cases
    (0                "  0 B" "  0 Bytes"    "   0 B" "   0 Bytes")
    (0.0              "  0 B" "  0 Bytes"    "   0 B" "   0 Bytes")
    (-0.0             "  0 B" "  0 Bytes"    "   0 B" "   0 Bytes")
    (1                "  1 B" "  1 Byte"     "+  1 B" "+  1 Byte")
    (-1               "  1 B" "  1 Byte"     "-  1 B" "-  1 Byte")
    ;; Rounding
    (-0.6             "0.6 B" "0.6 Bytes"    "-0.6 B" "-0.6 Bytes")
    (-0.5             "0.5 B" "0.5 Bytes"    "-0.5 B" "-0.5 Bytes")
    (-0.4             "0.4 B" "0.4 Bytes"    "-0.4 B" "-0.4 Bytes")
    ( 0.4             "0.4 B" "0.4 Bytes"    "+0.4 B" "+0.4 Bytes")
    ( 0.5             "0.5 B" "0.5 Bytes"    "+0.5 B" "+0.5 Bytes")
    ( 0.6             "0.6 B" "0.6 Bytes"    "+0.6 B" "+0.6 Bytes")
    ;; Corner cases
    (9                "  9 B" "  9 Bytes"    "+  9 B" "+  9 Bytes")
    (-9               "  9 B" "  9 Bytes"    "-  9 B" "-  9 Bytes")
    (9.001            "9.0 B" "9.0 Bytes"    "+9.0 B" "+9.0 Bytes")
    (10               " 10 B" " 10 Bytes"    "+ 10 B" "+ 10 Bytes")
    (999              "999 B" "999 Bytes"    "+999 B" "+999 Bytes")
    (1000             "  1KB" "  1 kiloByte" "+  1KB" "+  1 kiloByte")
    (10000000/1001299 " 10 B" " 10 Bytes"    "+ 10 B" "+ 10 Bytes"))

  (define-print-human-readable-test
      (print-human-readable-duration/smoke print-human-readable-duration)
    ;; Not a suitable input
    ("foo"            "  foo" "       foo"       "   foo" "        foo")
    ;; Special cases
    (0                "  0 s" "  0 seconds"      "   0 s" "   0 seconds")
    (0.0              "  0 s" "  0 seconds"      "   0 s" "   0 seconds")
    (-0.0             "  0 s" "  0 seconds"      "   0 s" "   0 seconds")
    (1                "  1 s" "  1 second"       "+  1 s" "+  1 second")
    (-1               "  1 s" "  1 second"       "-  1 s" "-  1 second")
    ;; Rounding
    (-0.6001          "600ms" "600 milliseconds" "-600ms" "-600 milliseconds")
    (-0.4999          "500ms" "500 milliseconds" "-500ms" "-500 milliseconds")
    (-0.4             "400ms" "400 milliseconds" "-400ms" "-400 milliseconds")
    ( 0.4             "400ms" "400 milliseconds" "+400ms" "+400 milliseconds")
    ( 0.5             "500ms" "500 milliseconds" "+500ms" "+500 milliseconds")
    ( 0.6             "600ms" "600 milliseconds" "+600ms" "+600 milliseconds")
    ;; Corner cases
    (9                "  9 s" "  9 seconds"      "+  9 s" "+  9 seconds")
    (-9               "  9 s" "  9 seconds"      "-  9 s" "-  9 seconds")
    (9.001            "9.0 s" "9.0 seconds"      "+9.0 s" "+9.0 seconds")
    (10               " 10 s" " 10 seconds"      "+ 10 s" "+ 10 seconds")
    (999              " 17 m" " 17 minutes"      "+ 17 m" "+ 17 minutes")
    (1000             " 17 m" " 17 minutes"      "+ 17 m" "+ 17 minutes")
    (10000000/1001299 " 10 s" " 10 seconds"      "+ 10 s" "+ 10 seconds")))

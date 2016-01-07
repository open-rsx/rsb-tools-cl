;;;; orders-of-magnitude.lisp --- Determining and printing orders of magnitude.
;;;;
;;;; Copyright (C) 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.formatting)

(define-constant +orders-of-magnitude+
    (mapcar (lambda+ ((exponent long short))
              (list (expt 10 exponent) long short))
            `((-15 "~@[ ~*~]femto~@[~A~P~]" "~@[ ~*~]f~@[~A~]")
              (-12 "~@[ ~*~]pico~@[~A~P~]"  "~@[ ~*~]p~@[~A~]")
              ( -9 "~@[ ~*~]nano~@[~A~P~]"  "~@[ ~*~]n~@[~A~]")
              ( -6 "~@[ ~*~]micro~@[~A~P~]" "~@[ ~*~]µ~@[~A~]")
              ( -3 "~@[ ~*~]milli~@[~A~P~]" "~@[ ~*~]m~@[~A~]")
              (  0 "~@[ ~*~]~@[~A~P~]"      "~@[ ~*~] ~@[~A~]")
              (  3 "~@[ ~*~]kilo~@[~A~P~]"  "~@[ ~*~]K~@[~A~]")
              (  6 "~@[ ~*~]mega~@[~A~P~]"  "~@[ ~*~]M~@[~A~]")
              (  9 "~@[ ~*~]giga~@[~A~P~]"  "~@[ ~*~]G~@[~A~]")
              ( 12 "~@[ ~*~]tera~@[~A~P~]"  "~@[ ~*~]T~@[~A~]")
              ( 15 "~@[ ~*~]exo~@[~A~P~]"   "~@[ ~*~]E~@[~A~]")))
  :test #'equal)

(define-constant +non-negative-orders-of-magnitude+
    (subseq +orders-of-magnitude+ 5)
  :test #'equal)

(define-constant +duration-orders-of-magnitude+
    `(,@(subseq +orders-of-magnitude+ 0 6)
      (60                "~@[ ~*~]~*minute~P" "~* m~2*")
      (,(* 60 60)        "~@[ ~*~]~*hour~P"   "~* h~2*")
      (,(* 24 60 60)     "~@[ ~*~]~*day~P"    "~* d~2*")
      ;; Months are uncommon for this
      ;; (,(* 30 24 60 60)  "~@[ ~*~]~*month~P"  "~@[ ~*~]M ~2*")
      (,(* 365 24 60 60) "~@[ ~*~]~*year~P"   "~* y~2*"))
  :test #'equal)

(defun human-readable-value-and-suffix (value table)
  ;; Test dividing VALUE by successively larger factors in TABLE until
  ;; the result is in the desirable range or TABLE runs out.
  (iter (with value = (rationalize value))
        (with zero? = (zerop value))
        (for ((factor1 long short) (factor2)) :on table)
        (for (values quotient remainder) = (round value factor1))
        ;; If the quotient is in the desirable range, stop the
        ;; search and return.
        (cond
          ;; Special-case the value 0.
          ((and zero? (= 1 factor1))
           (return (values t 0 short long)))
          ;; QUOTIENT is in the desirable range [0, 9] => return.
          ((and (not zero?) (<= 0 quotient 9))
           (return (values (zerop remainder) (/ value factor1) short long)))
          ;; If QUOTIENT is in the desirable range
          ;; ]9, <smallest value that would result in >= 1 w.r.t. FACTOR2>[
          ;; => return. If not, check the other reasons for
          ;; returning and maybe return.
          ((or (not factor2) ; this is the last available factor
               (and (not zero?) (zerop quotient)) ; VALUE is too small for available factors
               (< 9 quotient (/ factor2 factor1))) ; < 1 for w.r.t next factor
           (return (values t quotient short long))))))

(defun print-human-readable-value (stream value
                                   &key
                                   (orders-of-magnitude +orders-of-magnitude+)
                                   signum? space? unit long?)
  (if (realp value)
      (let+ (((&values integer? value1 short-format long-format)
              (human-readable-value-and-suffix
               (abs value) orders-of-magnitude))
             (signum (when signum? (signum value))))
        (format stream "~[~;-~; ~;+~]~:[~,1F~;~3D~]~?"
                (if signum (+ (floor signum) 2) 0) integer? value1
                (if long? long-format short-format)
                (list space? unit value1)))
      (let ((width (+ (if signum? 1 0) 3 1 (length unit))))
        (format stream "~V@A" width value))))

(macrolet
    ((define-print-function (suffix &key table unit)
       (let ((name (symbolicate 'print-human-readable- suffix)))
         `(defun ,name (stream value &optional colon? at?)
            (let (,@(when unit `((unit (if at? ,@unit)))))
              (print-human-readable-value
               stream value
               ,@(when table `(:orders-of-magnitude ,table))
               ,@(when unit `(:unit unit))
               :signum? colon? :space? at? :long? at?))))))

  (define-print-function order-of-magnitude)
  (define-print-function count :table +non-negative-orders-of-magnitude+)
  (define-print-function size
      :table +non-negative-orders-of-magnitude+
      :unit  ("Byte" "B"))
  (define-print-function duration
      :table +duration-orders-of-magnitude+
      :unit  ("second" "s")))

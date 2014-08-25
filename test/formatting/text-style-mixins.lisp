;;;; text-style--mixins.lisp --- Unit tests for the text style mixin classes.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.formatting.test)

;;; Mock formatter class

(defclass mock-style-separator (separator-mixin)
  ()
  (:documentation
   "Unit tests for the `separator-mixin' formatting style class."))

(defmethod format-event ((event  event)
                         (style  mock-style-separator)
                         (stream stream)
                         &key &allow-other-keys)
  (princ #\* stream))

;;; `separator-mixin'

(deftestsuite separator-mixin-root (formatting-root)
  ()
  (:documentation
   "Unit tests for the `separator-mixin' class."))

(addtest (separator-mixin-root
          :documentation
          "Test constructing `separator-mixin' instances.")
  construction

  (ensure-cases (args expected)
      '(((:separator 5)            :error)
        ((:separator (:hrule #\-)) :error)
        ((:separator (:rule))      :error)

        ((:separator "bla")        nil)
        ((:separator ("a" #\-))    nil)
        ((:separator (:rule #\=))  nil))

    (when (eq expected :error)
      (ensure-condition 'error
        (apply #'make-instance 'separator-mixin args)))))

(addtest (separator-mixin-root
          :documentation
          "Test some simple cases of formatting events using methods
on `format-event' for `separator-mixin'.")
  smoke

  (ensure-style-cases (mock-style-separator)
    `((:separator nil)
      (,(make-event "/a" "b") ,(make-event "/c" "d"))
      "\\*\\*")

    `((:separator (:rule #\-))
      (,(make-event "/a" "b") ,(make-event "/c" "d"))
      "--------------------------------------------------------------------------------\\*--------------------------------------------------------------------------------\\*")

    `((:separator "&&&")
      (,(make-event "/a" "b") ,(make-event "/c" "d"))
      "&&&\\*&&&\\*")))

;;; `width-specification-mixin'

(deftestsuite width-specification-mixin-root (formatting-root)
  ()
  (:documentation
   "Test suite for the `width-specification-mixin' mixin class."))

(addtest (width-specification-mixin-root
          :documentation
          "Test constructing `width-specification-mixin' instances.")
  construction

  (ensure-cases (initargs expected)
      '(;; Some invalid initarg combinations.
        (()                       missing-required-initarg)
        ((:priority 3)            missing-required-initarg)
        ((:width 1 :widths 2)     incompatible-initargs)
        ;; Invalid width specifications.
        ((:widths :foo)           type-error)
        ((:widths (1 :foo))       type-error)
        ((:widths (:range 2 1))   type-error)
        ;; These are valid.
        ((:width 0)               t)
        ((:width 1)               t)
        ((:widths (1))            t)
        ((:widths (1 2))          t)
        ((:widths (:range 1))     t)
        ((:widths (:range 1 2))   t)
        ((:widths ((:range 1)))   t)
        ((:widths ((:range 1 2))) t))

    (flet ((do-it ()
             (apply #'make-instance 'width-specification-mixin
                    initargs)))
      (case expected
        (missing-required-initarg
         (ensure-condition missing-required-initarg (do-it)))
        (incompatible-initargs
         (ensure-condition incompatible-initargs (do-it)))
        (type-error
         (ensure-condition type-error (do-it)))
        (t
         (do-it))))))

;;; `width-mixin'

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

;;; `columns-mixin'

(deftestsuite columns-mixin-root (formatting-root)
  ()
  (:documentation
   "Test suite for the `columns-mixin' mixin class."))

(addtest (columns-mixin-root
          :documentation
          "Test construction of the `columns-mixin' class.")
  construction

  (ensure-cases (args expected)
      '(;; Invalid constructions
        ((:columns (5))                     :error)
        ((:columns (()))                    :error)
        ((:columns (:no-such-column-class)) :error)

        ;; These are ok
        (()                                 nil)
        ((:columns   ())                    nil)
        ((:separator "=")                   nil)
        ((:columns   (:mock))               (mock-column))
        ((:columns   ((:mock :width 2)))    (mock-column)))

    (if (eq expected :error)
        (ensure-condition 'error
          (apply #'make-instance 'columns-mixin args))
        (let ((instance (apply #'make-instance 'columns-mixin args)))
          (ensure-same (map 'list #'class-of (style-columns instance))
                       (map 'list #'find-class expected)
                       :test #'equal)))))

(addtest (columns-mixin-root
          :documentation
          "Test the method on `format-event' for `columns-mixin' which
           should format events according to the column specification
           of the style.")
  format-event

  (ensure-style-cases (columns-mixin)
    ;; no columns, no events => no output
    '(()
      ()
      "")

    ;; events, but no columns => no output
    '(()
      ("/foo/bar")
      "")

    ;; single column => separator should not be printed
    '((:columns ((:mock :width 16 :alignment :left)) :separator "!")
      ("/foo/bar/")
      "/foo/bar/       ")

    ;; two columns => separator should be printed
    '((:columns   ((:mock :width 16 :alignment :right)
                   (:mock :width 8  :alignment :left))
       :separator "!")
      ("/foo/bar/")
      "       /foo/bar/!/foo/ba…")))

(addtest (columns-mixin-root
          :documentation
          "Test the method on `format-header' for `columns-mixin'
           which should format headers according to the column
           specification of the style.")
  format-header

  (ensure-style-cases (columns-mixin :formatter :format-header)
    ;; no columns, no events => no output
    '(()
      ()
      "")

    ;; events, but no columns => no output
    '(()
      ("/foo/bar")
      "")

    ;; single column => separator should not be printed
    '((:columns ((:mock :width 16 :alignment :left)) :separator "!")
      ("/foo/bar/")
      "My mock header  ")

    ;; two columns => separator should be printed
    '((:columns   ((:mock :width 16 :alignment :right)
                   (:mock :width 8  :alignment :left))
       :separator "!")
      ("/foo/bar/")
      "My mock header  !My mock…")))

;; Local Variables:
;; coding: utf-8
;; End:

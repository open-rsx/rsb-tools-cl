;;;; style-columns.lisp --- Unit tests for the columns formatting style.
;;;;
;;;; Copyright (C) 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.formatting.test)

(deftestsuite style-columns-root (formatting-root)
  ()
  (:documentation
   "Unit tests for the `style-columns' formatting style class."))

(addtest (style-columns-root
          :documentation
          "Test some simple cases of formatting events using methods
           on `format-event' for `style-columns'.")
  smoke

  (let ((*print-right-margin* 80))
    (ensure-style-cases (style-columns)
      '(()
        ()
        "")

      `((:header-frequency nil :columns ((:scope :width 10)))
        (,(make-event "/foo" "bar"))
        "/foo/     ")

      `((:header-frequency nil :columns ((:scope :width 10) (:data :width 10)))
        (,(make-event "/foo" "bar"))
        "/foo/     │\"bar\"     ")

      `((:header-frequency 1 :columns ((:scope :width 10) :newline))
        (,(make-event "/foo" "bar"))
        "Scope     
/foo/     
"))))

;; Local Variables:
;; coding: utf-8
;; End:

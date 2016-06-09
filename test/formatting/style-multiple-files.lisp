;;;; style-multiple-files.lisp --- Unit tests for the multiple file style.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.formatting.test)

(deftestsuite style-multiple-files-root (formatting-root)
  ()
  (:documentation
   "Unit tests for the `style-multiple-files' formatting style
    class."))

(addtest (style-multiple-files-root
          :documentation
          "Test constructing instances of the `style-multiple-files'
           formatting style class.")
  construct

  (ensure-cases (initargs &optional expected)
      '(;; Some invalid cases.
        (()                              missing-required-initarg)
        ((:filename-template "${count}") missing-required-initarg)
        ((:filename-style    :detailed)  missing-required-initarg)
        ((:event-style       :detailed)  missing-required-initarg)

        ((:filename-template "${count}"
          :filename-style    :detailed
          :event-style       :detailed)
                                         incompatible-initargs)

        ;; These are OK.
        ((:filename-template "${count}"
          :event-style       :detailed))
        ((:filename-style :detailed
          :event-style    :detailed)))

    (flet ((do-it ()
             (apply #'make-instance 'style-multiple-files initargs)))
      (case expected
        (missing-required-initarg
         (ensure-condition missing-required-initarg (do-it)))
        (incompatible-initargs
         (ensure-condition incompatible-initargs (do-it)))
        (t
         (do-it))))))

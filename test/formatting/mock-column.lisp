;;;; mock-column.lisp --- A mock column class for tests.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.formatting.test)

(defmethod find-column-class ((spec (eql :mock)))
  (find-class 'mock-column))

(defclass mock-column (width-mixin)
  ())

(defmethod format-header ((thing  mock-column)
                          (target t))
  (format target "My mock header"))

(defmethod format-event ((event  t)
                         (style  mock-column)
                         (target t)
                         &key &allow-other-keys)
  (format target "~A" event))

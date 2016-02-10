;;;; mock-column.lisp --- A mock column class for tests.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.formatting.test)

(defclass mock-column (width-mixin)
  ())

(service-provider:register-provider/class
 'rsb.formatting::column :mock :class 'mock-column)

(defmethod format-header ((thing  mock-column)
                          (target t))
  (format target "My mock header"))

(defmethod format-event ((event  t)
                         (style  mock-column)
                         (target t)
                         &key &allow-other-keys)
  (format target "~A" event))

;;;; name-mixin.lisp --- Mixin class for named column classes.
;;;;
;;;; Copyright (C) 2011, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.formatting)

(defclass name-mixin ()
  ((name :initarg  :name
         :type     string
         :accessor column-name
         :documentation
         "Stores the name of the column."))
  (:default-initargs
   :name (missing-required-initarg 'name-mixin :name))
  (:documentation
   "This class is intended to be mixed into column classes in order to
    handle the column name."))

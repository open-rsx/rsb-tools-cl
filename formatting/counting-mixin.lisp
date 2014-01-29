;;;; counting-mixin.lisp --- Output counting style mixin class.
;;;;
;;;; Copyright (C) 2011, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.formatting)

(defclass counting-mixin ()
  ((count :initarg  :count
          :type     non-negative-integer
          :accessor style-count
          :initform 0
          :documentation
          "Stores the number of output cycles already performed by the
           formatter."))
  (:documentation
   "This class is intended to be mixed into formatter classes that
    need keep track of the number of performed output cycles."))

(defmethod format-event :after ((event  t)
                                (style  counting-mixin)
                                (stream t)
                                &key &allow-other-keys)
  (incf (style-count style)))

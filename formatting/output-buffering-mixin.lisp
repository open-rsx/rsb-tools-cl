;;;; output-buffering-mixin.lisp --- Mixin for buffering of formatter output.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsb.formatting)

(defclass output-buffering-mixin ()
  ()
  (:documentation
   "This mixin class provides buffering of output. This can be useful
when lots of output has to be produced and written. This can, for
example, reduce flickering."))

(defmethod format-event :around ((event  (eql :trigger))
                                 (style  output-buffering-mixin)
                                 (stream t)
                                 &rest args &key &allow-other-keys)
  (write-string
   (with-output-to-string (stream)
     (apply #'call-next-method event style stream args))
   stream))

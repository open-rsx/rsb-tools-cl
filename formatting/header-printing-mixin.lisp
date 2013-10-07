;;;; header-printing-mixin.lisp --- Mixin for formatting styles which print headers.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsb.formatting)

(defclass header-printing-mixin (counting-mixin)
  ((header-frequency :initarg  :header-frequency
                     :type     (or null positive-integer)
                     :accessor style-header-frequency
                     :initform 22
                     :documentation
                     "Stores the number of output cycles after which a
header should be printed or nil in case a header is never printed."))
  (:documentation
   "This class is intended to be mixed into formatting style classes
that periodically print a header of some kind into their regular
stream of output."))

(defmethod format-event :before ((event  t)
                                 (style  header-printing-mixin)
                                 (stream t)
                                 &key &allow-other-keys)
  (let+ (((&accessors-r/o (header-frequency style-header-frequency)) style))
    (when (and header-frequency
               (zerop (mod (style-count style) header-frequency)))
      (format-header style stream))))

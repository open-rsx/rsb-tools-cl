;;;; event-style-columns.lisp --- Generic column-based formatting class.
;;;;
;;;; Copyright (C) 2011, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.formatting)

(defclass style-columns (header-printing-mixin
                         columns-mixin)
  ()
  (:documentation
   "This formatting style prints configurable properties of received
    events in a column-oriented fashion. Event properties and the
    associated columns in which the properties should be printed have
    to be specified using the :columns initarg. If no columns are
    specified, no output is produced."))

(service-provider:register-provider/class
 'style :columns :class 'style-columns)

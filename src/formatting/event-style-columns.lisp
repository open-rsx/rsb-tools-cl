;;;; event-style-columns.lisp --- Generic column-based formatting class.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
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

;; TODO almost identical method in event-style-statistics
(defmethod format-event :before ((event t) (style style-columns) (stream t)
                                 &key
                                 (width (or *print-right-margin* 80)))
  (let+ (((&structure-r/o style- dynamic-width-columns separator-width) style)
         (widths (style-compute-column-widths
                  style dynamic-width-columns width
                  :separator-width separator-width)))
    (style-assign-column-widths style dynamic-width-columns widths)))

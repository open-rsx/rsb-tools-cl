;;;; event-style-detailed.lisp --- Detailed event formatting style class.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.formatting)

(defclass style-detailed (style-meta-data)
  ()
  (:documentation
   "Format each event on multiple lines with as many details as
    possible."))

(service-provider:register-provider/class
 'style :detailed :class 'style-detailed)

(defmethod format-event ((event  event)
                         (style  style-detailed)
                         (stream t)
                         &key
                         (max-lines   16)
                         (max-columns (or *print-right-margin* 80)))
  ;; Meta-data.
  (call-next-method)

  ;; Payload.
  (let+ (((&accessors-r/o (data event-data)) event))
    (when (> max-lines 11)
      (with-indented-section (stream (format nil "Payload (~S)"
                                             (class-name (class-of data))))
        (format-payload data :any stream
                        :max-lines   (- max-lines 11)
                        :max-columns (- max-columns 2))))))

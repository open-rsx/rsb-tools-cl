;;;; event-style-detailed.lisp --- Detailed event formatting style class.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014, 2015 Jan Moringen
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
  (pprint-logical-block (stream (list event))
    ;; Meta-data.
    (call-next-method)

    ;; Payload.
    (let+ ((data (event-data event))
           ((&values type length unit) (event-payload-description event)))
      (format stream "Payload: ~A~:[~*~;, ~:*~:D ~(~A~)~2:*~P~*~]~
                      ~@:_~2@T"
              (typecase type
                ((and symbol (not keyword))
                 (with-standard-io-syntax (prin1-to-string type)))
                (t
                 type))
              length unit)
      (pprint-logical-block (stream (list data))
        (format-payload data :any stream
                             :max-lines   (- max-lines 11)
                             :max-columns (- max-columns 2)))))
  (terpri stream))

;;;; event-style-detailed.lisp --- Detailed event formatting style class.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.formatting)

(defclass style-detailed (meta-data-mixin
                          payload-style-mixin
                          separator-mixin
                          max-lines-mixin)
  ()
  (:default-initargs
   :payload-style :payload-generic/pretty
   :separator     `(#\Newline
                    (:rule ,(if *textual-output-can-use-utf-8?* #\─ #\-))
                    #\Newline)
   :max-lines     200)
  (:documentation
   "Format each event on multiple lines with as many details as
    possible."))

(service-provider:register-provider/class
 'style :detailed :class 'style-detailed)

(defmethod format-event ((event  event)
                         (style  style-detailed)
                         (stream t)
                         &key)
  (pprint-logical-block (stream (list event))
    ;; Meta-data.
    (call-next-method)

    ;; Payload.
    (when (or (not *print-lines*) (> *print-lines* 13))
      (let+ ((data (event-data event))
             ((&values type length unit) (event-payload-description event))
             (*print-lines* (when *print-lines*
                              (max 0 (- *print-lines* 11)))))
        (format stream "~@:_~
                        Payload: ~A~:[~*~;, ~:*~:D ~(~A~)~2:*~P~*~]~
                        ~@:_~2@T"
                (typecase type
                  ((and symbol (not keyword))
                   (with-standard-io-syntax (prin1-to-string type)))
                  (t
                   type))
                length unit)
        (pprint-logical-block (stream (list data))
          (format-payload data (style-payload-style style) stream))))))

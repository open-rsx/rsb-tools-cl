;;;; event-style-multiple-files.lisp --- Write to one file per event.
;;;;
;;;; Copyright (C) 2012, 2013, 2014, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.formatting)

(defclass event-style-multiple-files ()
  ((filename-style :initarg  :filename-style
                   :reader   style-filename-style
                   :writer   (setf style-%filename-style)
                   :documentation
                   "Stores an event formatting style for producing
                    output file names.

                    `format-event' is called with the event, this
                    style and a `string-stream'. The style then prints
                    the desired output file name to the stream.")
   (event-style    :initarg  :event-style
                   :reader   style-event-style
                   :writer   (setf style-%event-style)
                   :documentation
                   "Stores and event formatting style for producing
                    the content of output file.

                    `format-event' is called with the event, this
                    style and a stream connected to the output file
                    for the event."))
  (:default-initargs
   :filename-style (missing-required-initarg
                    'event-style-multiple-files :filename-style)
   :event-style    (missing-required-initarg
                    'event-style-multiple-files :event-style))
  (:documentation
   "Write style output to a new files for each event."))

(service-provider:register-provider/class
 'style :multiple-files :class 'event-style-multiple-files)

(defmethod shared-initialize :after ((instance   event-style-multiple-files)
                                     (slot-names t)
                                     &key
                                     filename-style
                                     event-style)
  (when filename-style
    (setf (style-%filename-style instance) (ensure-style filename-style)))
  (when event-style
    (setf (style-%event-style instance) (ensure-style event-style))))

(defmethod format-event ((event  t)
                         (style  event-style-multiple-files)
                         (target t)
                         &rest args &key)
  (let+ (((&structure-r/o style- filename-style event-style) style)
         ((pathname &rest open-args &key (element-type :default))
          (ensure-list
           (with-output-to-string (stream)
             (format-event event filename-style stream)))))
    (with-open-stream (stream (apply #'open pathname
                                     :direction    :output
                                     :element-type element-type
                                     (remove-from-plist
                                      open-args :element-type)))
      (apply #'format-event event event-style stream args))))



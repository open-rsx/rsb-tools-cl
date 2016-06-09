;;;; event-style-multiple-files.lisp --- Write to one file per event.
;;;;
;;;; Copyright (C) 2012, 2013, 2014, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.formatting)

;;; `style-multiple-files'

(defclass style-multiple-files ()
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
   :event-style (missing-required-initarg
                 'style-multiple-files :event-style))
  (:documentation
   "Write style output to a new file for each event.

    The filename style is applied to events to determine the name of
    the file into which the respective event should be written.

    The event style is applied to events to produce the content of
    these files."))

(service-provider:register-provider/class
 'style :multiple-files :class 'style-multiple-files)

(defmethod shared-initialize :before
    ((instance   style-multiple-files)
     (slot-names t)
     &key
     (filename-template nil filename-template-supplied?)
     (filename-style    nil filename-style-supplied?))
  (cond
    ((not (or filename-template-supplied? filename-style-supplied?))
     (missing-required-initarg 'style-multiple-files
                               :filename-style-xor-filename-template))
    ((and filename-template-supplied? filename-style-supplied?)
     (incompatible-initargs 'style-multiple-files
                            :filename-template filename-template
                            :filename-style    filename-style))))

(defmethod shared-initialize :after ((instance   style-multiple-files)
                                     (slot-names t)
                                     &key
                                     filename-template
                                     filename-style
                                     event-style)
  (cond
    (filename-template
     (setf (style-%filename-style instance)
           (ensure-style `(:programmable/template
                           :template ,filename-template))))
    (filename-style
     (setf (style-%filename-style instance) (ensure-style filename-style))))
  (when event-style
    (setf (style-%event-style instance) (ensure-style event-style))))

(defmethod rsb.ep:access? ((processor style-multiple-files)
                           (part      t)
                           (mode      t))
  (let+ (((&structure-r/o style- filename-style event-style) processor))
    (or (rsb.ep:access? filename-style part mode)
        (rsb.ep:access? event-style part mode))))

(defmethod format-event ((event  t)
                         (style  style-multiple-files)
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

;;; `style-raw-files'

(defclass style-raw-files (style-multiple-files)
  ()
  (:default-initargs
   :filename-template "${count}.bin"
   :event-style       '(:payload
                        :separator     nil
                        :payload-style :payload-generic/raw))
  (:documentation
   "Write raw payload data to a new file for each event."))

(service-provider:register-provider/class
 'style :raw-files :class 'style-raw-files)

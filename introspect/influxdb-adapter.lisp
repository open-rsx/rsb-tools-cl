;;;; influxdb-adapter.lisp --- Push introspection data into an InfluxDB.
;;;;
;;;; Copyright (C) 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.tools.introspect)

(defclass style-influxdb-adapter (database-mixin
                                  rsb.formatting:periodic-printing-mixin)
  ((host     :initarg  :host
             :type     (or null string)
             :reader   style-host
             :initform "localhost"
             :documentation
             "Host on which the InfluxDB instance is running.")
   (port     :initarg  :port
             :type     integer
             :reader   style-port
             :initform 8086
             :documentation
             "Port on which the HTTP endpoint of the InfluxDB
              instance is listening.")
   (db       :initarg  :db
             :type     (or null string)
             :reader   style-db
             :initform nil
             :documentation
             "Name of the InfluxDB database into which the
              introspection summary data should be pushed.")
   (username :initarg  :username
             :type     (or null string)
             :reader   style-username
             :initform "root"
             :documentation
             "Username to use for HTTP basic authentication with the
              InfluxDB endpoint.")
   (password :initarg  :password
             :type     (or null string)
             :reader   style-password
             :initform "root"
             :documentation
             "Password to use for HTTP basic authentication with the
              InfluxDB endpoint."))
  (:default-initargs
   :print-interval 4
   :db             (missing-required-initarg 'style-influxdb-adapter :db))
  (:documentation
   "Pushes aggregated introspection information into an InfluxDB instance.

    Data is transferred via HTTP POST requests to the InfluxDB
    endpoint described by HOST, PORT and DB. USERNAME and PASSWORD, if
    supplied are used for HTTP basic authentication when communicating
    with the endpoint."))

(service-provider:register-provider/class
 'rsb.formatting.introspection::style :influxdb-adapter :class 'style-influxdb-adapter)

(defmethod detach ((participant style-influxdb-adapter)))

;; Introspection event => ignore.
(defmethod rsb.ep:handle ((sink style-influxdb-adapter) (data list)))

;; Dummy event => return true to indicate that the program should
;; continue.
(defmethod rsb.formatting:format-event ((event  t)
                                        (style  style-influxdb-adapter)
                                        (target t)
                                        &key &allow-other-keys)
  t)

(defmethod rsb.formatting:format-event ((event  (eql :trigger))
                                        (style  style-influxdb-adapter)
                                        (target t)
                                        &key &allow-other-keys)
  (let+ (((&structure-r/o style- database host port db username password) style)
         (base-url (make-instance 'puri:uri
                                  :scheme :http
                                  :host   host
                                  :port   port
                                  :path   (format nil "/db/~A/series" db)))
         (credentials (when (and username password)
                        (list username password)))
         ((&flet post (series columns values)
            "Push a set the datapoints described by COLUMNS and VALUES
             into SERIES in the database."
            (apply #'drakma:http-request
             base-url
             :method  :post
             :content (json:encode-json-to-string
                       `(((:name    . ,series)
                          (:columns . ,columns)
                          (:points  . ,values))))
             (when credentials
               (list :basic-authorization credentials)))))
         ((&flet timing-information (info &optional clock-offset?)
            (values
             (if clock-offset? '(#1="latency" "clock_offset") '(#1#))
             (list* (rsb.introspection:info-latency info)
                    (when clock-offset?
                      (list (rsb.introspection:info-clock-offset info)))))))
         ((&flet count-participants (entry)
            (let+ ((participants (rsb.introspection:introspection-participants entry))
                   ((&flet count-of-kind (kind)
                      (count-if
                       (lambda (entry)
                         (let ((info (rsb.introspection:entry-info entry)))
                           (eq (rsb.introspection:participant-info-kind info) kind)))
                       participants)))
                   (kinds '(:listener :informer :reader
                            :local-server  :local-method
                            :remote-server :remote-method)))
              (values
               (list* "participants"
                      (mapcar (curry #'format-safe-name
                                     nil "~(~A~)_participants")
                              kinds))
               (list* (length participants)
                      (mapcar #'count-of-kind kinds))))))
         ((&flet one-process (host-entry process-entry)
            "POST information for the process PROCESS-ENTRY and its
             participants."
            (let+ ((hostname   (rsb.introspection:host-info-hostname
                                (rsb.introspection:entry-info host-entry)))
                   (info       (rsb.introspection:entry-info process-entry))
                   (program-name (simplify-program-name
                                  (rsb.introspection:process-info-program-name info)))
                   (process-id (rsb.introspection:process-info-process-id info))
                   ((&values timing-columns timing-values)
                    (timing-information info))
                   ((&values participant-columns participant-values)
                    (count-participants process-entry)))
              (post (format-safe-name nil "introspection_process_~A_~A_~D"
                                      hostname program-name process-id)
                          (append '("pid")          participant-columns timing-columns)
                    (list (append (list process-id) participant-values  timing-values))))))
         ((&flet count-processes (entry)
            "Return values that reflect the number of processes in
             specific states."
            (let+ ((processes (rsb.introspection:entry-children entry))
                   ((&flet count-in-state (state)
                      (count-if
                       (lambda (entry)
                         (let ((info (rsb.introspection:entry-info entry)))
                           (eq (rsb.introspection:process-info-state info)
                               state)))
                       processes)))
                  (states '(:crashed)))
             (values (mapcar (curry #'format-safe-name
                                    nil "~(~A~)_processes")
                             states)
                     (mapcar #'count-in-state states)))))
         ((&flet one-host (entry)
            "POST information for the host ENTRY and its processes."
            (let+ ((info (rsb.introspection:entry-info entry))
                   (hostname (rsb.introspection:host-info-hostname info))
                   ((&values timing-columns timing-values)
                    (timing-information info t))
                   ((&values process-columns process-values)
                    (count-processes entry)))
              (post (format-safe-name nil "introspection_host_~A" hostname)
                          (append process-columns timing-columns)
                    (list (append process-values  timing-values))))
            (mapc (curry #'one-process entry)
                  (rsb.introspection:entry-children entry)))))
    (rsb.introspection:with-database-lock (database)
      (mapc #'one-host (rsb.introspection:entry-children
                        (rsb.introspection::introspection-database database))))))

;;; Utility functions

(defun make-safe-name (string)
  (substitute-if-not #\_ (lambda (character)
                           (or (char= character #\_)
                               (alphanumericp character)))
                     string))

(defun format-safe-name (stream format-control &rest format-arguments)
  (make-safe-name (apply #'format stream format-control format-arguments)))

(defun simplify-program-name (name)
  (let+ (((name &optional rest)
          (split-sequence:split-sequence #\Space name
                                         :count    2
                                         :from-end t))
         ((&flet relativise (pathname)
            (make-pathname :directory nil
                           :defaults  pathname))))
    (format nil "~A~@[ ~A~]"
            (relativise name)
            (when rest (relativise rest)))))

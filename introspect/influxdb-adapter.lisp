;;;; influxdb-adapter.lisp --- Push introspection data into an InfluxDB.
;;;;
;;;; Copyright (C) 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.formatting.introspection)

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
   :db             (more-conditions:missing-required-initarg 'style-influxdb-adapter :db))
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

(define-constant +participant-kinds+
    '(:listener :informer :reader
      :local-server  :local-method
      :remote-server :remote-method)
  :test #'equal)

(defun participant-kind-columns () ; TODO constant
  (mapcar (curry #'format nil "~(~A~)_participants")
                              +participant-kinds+))

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
         ((&flet post (series columns &rest values)
            "Push a set the datapoints described by COLUMNS and VALUES
             into SERIES in the database."
            (let+ (((&values reply code)
                    (apply #'drakma:http-request
                     base-url
                     :method  :post
                     :content (json:encode-json-to-string
                               `(((:name    . ,series)
                                  (:columns . ,columns)
                                  (:points  . ,values))))
                     (when credentials
                       (list :basic-authorization credentials)))))
              (unless (<= 200 code 299)
                (warn "~@<HTTP POST request failed with code ~D~@[: ~A~]~@:>"
                      code reply)))))
         ((&flet timing-information (info &optional clock-offset?)
            (values
             (if clock-offset? '(#1="latency" "clock_offset") '(#1#))
             (list* (rsb.model:info-latency info)
                    (when clock-offset?
                      (list (rsb.model:info-clock-offset info)))))))
         ((&flet count-participants (node)
            (declare (type rsb.model:process-node node))
            (let+ ((participants (rsb.introspection:introspection-participants node))
                   ((&flet count-of-kind (kind)
                      (count-if
                       (lambda (entry)
                         (let ((info (rsb.model:node-info entry)))
                           (eq (rsb.model:participant-info-kind info) kind)))
                       participants))))
              (values
               (list* "participants"
                      (participant-kind-columns))
               (list* (length participants)
                      (mapcar #'count-of-kind +participant-kinds+))))))
         ((&flet post-named-process (process-entry)
            "POST information for the process PROCESS-ENTRY and its
             participants."
            (let+ ((info         (rsb.model:node-info process-entry))
                   (process-id   (rsb.model:process-info-process-id info))
                   (program-name (simplify-program-name
                                  (rsb.model:process-info-program-name info)))
                   (display-name (rsb.model:process-info-display-name info))
                   ((&values timing-columns timing-values)
                    (timing-information info))
                   ((&values participant-columns participant-values)
                    (count-participants process-entry))
                   (series (format nil "introspection.process.~(~A~)" ; display-name is assumed to be unique
                                   (make-safe-name display-name))))
              (assert (not (emptyp display-name)))
              (post series
                    (append '(    "pid"      "program_name")
                            participant-columns timing-columns)
                    (append (list process-id program-name)
                            participant-values  timing-values)))))
         ((&flet post-unnamed-processes (hostname processes)
            (let+ (((columns counts)
                    (reduce (lambda+ ((&ign    result-counts)
                                      (columns process-counts))
                              (list columns (mapcar #'+ result-counts process-counts)))
                            processes
                            :key (lambda (process)
                                   (multiple-value-list
                                    (count-participants process)))
                            :initial-value (list (list* "participants"
                                                        (participant-kind-columns))
                                                 (make-list (1+ (length +participant-kinds+))
                                                            :initial-element 0)))))
              (post (format nil "introspection.unnamed_processes.~(~A~)"
                            (make-safe-name hostname))
                    (list* "count"            columns)
                    (list* (length processes) counts)))))
         ((&flet count-processes (node)
            "Return values that reflect the number of processes in
             specific states."
            (declare (type rsb.model:host-node node))
            (let+ ((processes (rsb.model:node-children node))
                   ((&flet count-in-state (state)
                      (count-if
                       (lambda (node)
                         (declare (type rsb.model:process-node node))
                         (let ((info (rsb.model:node-info node)))
                           (eq (rsb.model:process-info-state info) state)))
                       processes)))
                  (states '(:crashed)))
             (values (mapcar (curry #'format nil "processes.~(~A~)") states)
                     (mapcar #'count-in-state states)))))
         ((&flet one-host (node)
            "POST information for the host NODE and its processes."
            (declare (type rsb.model:host-node node))
            (let+ ((info     (rsb.model:node-info node))
                   (hostname (rsb.model:host-info-hostname info))
                   ((&values timing-columns timing-values)
                    (timing-information info t))
                   ((&values process-columns process-values)
                    (count-processes node))
                   (processes (rsb.model:node-children node))
                   (named-processes (remove-if-not (compose #'rsb.model:process-info-display-name
                                                            #'rsb.model:node-info)
                                                   processes))
                   (unnamed-processes (set-difference processes named-processes)))
              (post (format nil "introspection.host.~A" (make-safe-name hostname))
                    (append process-columns timing-columns)
                    (append process-values  timing-values))
              (mapc #'post-named-process named-processes)
              (post-unnamed-processes hostname unnamed-processes)))))
    (rsb.introspection:with-database-lock (database)
      (mapc #'one-host (rsb.model:node-children
                        (rsb.introspection::introspection-database database))))))

;;; Utility functions

(defun make-safe-name (string)
  (substitute-if-not #\_ (lambda (character)
                           (or (char= character #\_)
                               (alphanumericp character)))
                     string))

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

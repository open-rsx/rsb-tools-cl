;;;; graphite-adapter.lisp --- Push introspection data into an Graphite.
;;;;
;;;; Copyright (C) 2015, 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.formatting.introspection)

(defclass style-graphite-adapter (database-mixin
                                  rsb.formatting:periodic-printing-mixin)
  ((host     :initarg  :host
             :type     (or null string)
             :reader   style-host
             :initform "localhost"
             :documentation
             "Host on which the Carbon daemon is running.")
   (port     :initarg  :port
             :type     integer
             :reader   style-port
             :initform 2003
             :documentation
             "Port on which the plaintext endpoint of the Carbon
              daemon is listening."))
  (:default-initargs
   :print-interval 4)
  (:documentation
   "Pushes aggregated introspection information into a Graphite database.

    Data is transferred to the Carbon daemon listening on HOST and
    PORT."))

(service-provider:register-provider/class
 'rsb.formatting.introspection::style :graphite-adapter :class 'style-graphite-adapter)

(defmethod detach ((participant style-graphite-adapter)))

;; Introspection event => ignore.
(defmethod rsb.ep:handle ((sink style-graphite-adapter) (data list)))

;; Dummy event => return true to indicate that the program should
;; continue.
(defmethod rsb.formatting:format-event ((event  t)
                                        (style  style-graphite-adapter)
                                        (target t)
                                        &key &allow-other-keys)
  t)

(define-constant +participant-kinds+
    '(:listener :informer :reader
      :local-server  :local-method
      :remote-server :remote-method
      :connection    :bridge)
  :test #'equal)

(defun participant-kind-columns () ; TODO constant
  (mapcar (compose #'list (curry #'format nil "~(~A~)_participants")
                   #'make-safe-name
                   #'string)
          +participant-kinds+))

(defmethod rsb.formatting:format-event ((event  (eql :trigger))
                                        (style  style-graphite-adapter)
                                        (target t)
                                        &key &allow-other-keys)
  (let+ (((&structure-r/o style- database host port) style)
         (values (rsb.introspection:with-database-lock (database)
                   (prepare-values
                    (rsb.model:node-children
                     (rsb.introspection::introspection-database database))))))
    (submit-values host port values)))

;;; Utility functions

(defun submit-values (host port values)
  ;; Push a set the datapoints described by COLUMNS and VALUES into
  ;; SERIES in the database.
  (usocket:with-client-socket (socket stream host port)
    (format stream "~{~{~{~A~^.~} ~F ~D.0~}~%~}" values)))

(defun prepare-values (hosts
                       &key (timestamp (local-time:now)))
  (let+ ((timestamp (local-time:timestamp-to-unix timestamp))
         (result)
         ((&flet collect (&rest keys-and-values)
            (loop :for (key value) :on keys-and-values :by #'cddr :do
              (push (list key value timestamp) result))))
         ((&flet timing-information (info &optional clock-offset?)
            (values
             (if clock-offset? '(#1=("latency") ("clock_offset")) '(#1#))
             (append (when-let ((value (rsb.model:info-latency info)))
                       (list value))
                     (when clock-offset?
                       (when-let ((value (rsb.model:info-clock-offset info)))
                         (list value)))))))
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
               (list* '("participants")
                      (participant-kind-columns))
               (list* (length participants)
                      (mapcar #'count-of-kind +participant-kinds+))))))
         ((&flet named-process (process-entry)
            "Submit information for the process PROCESS-ENTRY and its
             participants."
            (let+ ((info         (rsb.model:node-info process-entry))
                   (process-id   (rsb.model:process-info-process-id info))
                   (display-name (rsb.model:process-info-display-name info))
                   ((&values timing-columns timing-values)
                    (timing-information info))
                   ((&values participant-columns participant-values)
                    (count-participants process-entry))
                   ((&flet series (key)
                      (list* "introspection" "process" ; display-name is assumed to be unique
                             (string-downcase (make-safe-name display-name))
                             key))))
              (assert (not (emptyp display-name)))
              (collect (series '("pid"))          process-id
                       #+no (series '("program_name")) #+no program-name)
              (mapc #'collect (mapcar #'series participant-columns) participant-values)
              (mapc #'collect (mapcar #'series timing-columns)      timing-values))))
         (user-processes (make-hash-table :test #'equal))
         ((&flet user-process (node)
            (declare (type rsb.model:process-node node))
            (let* ((info (rsb.model:node-info node))
                   (user (or (rsb.model:process-info-executing-user info)
                             "<unknown>")))
              (incf (gethash user user-processes 0)))))
         ((&flet unnamed-processes (hostname processes)
            (let+ (((columns counts)
                    (reduce (lambda+ ((&ign    result-counts)
                                      (columns process-counts))
                              (list columns (mapcar #'+ result-counts process-counts)))
                            processes
                            :key (lambda (process)
                                   (multiple-value-list
                                    (count-participants process)))
                            :initial-value (list (list* '("participants")
                                                        (participant-kind-columns))
                                                 (make-list (1+ (length +participant-kinds+))
                                                            :initial-element 0))))
                   ((&flet series (key)
                      (list* "introspection" "unnamed_processes"
                             (string-downcase (make-safe-name hostname))
                             key))))
              (collect (series '("count")) (length processes))
              (mapc #'collect (mapcar #'series columns) counts))))
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
              (values (mapcar (lambda (state) (list "processes" (string-downcase state))) states)
                      (mapcar #'count-in-state states)))))
         ((&flet one-host (node)
            "Submit information for the host NODE and its processes."
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
                   (unnamed-processes (set-difference processes named-processes))
                   ((&flet series (key)
                      (list* "introspection" "host"
                             (string-downcase (make-safe-name hostname)) key))))
              (mapc #'collect (mapcar #'series process-columns) process-values)
              (mapc #'collect (mapcar #'series timing-columns) timing-values)
              (mapc #'named-process named-processes)
              (mapc #'user-process processes)
              (unnamed-processes hostname unnamed-processes)))))
    (mapc #'one-host hosts)
    (maphash (lambda (user count)
               (collect (list "introspection" "user" user "processes") count))
             user-processes)
    result))

(defun make-safe-name (string)
  (substitute-if-not #\_ (lambda (character)
                           (or (char= character #\_)
                               (alphanumericp character)))
                     string))

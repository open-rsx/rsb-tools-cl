;;;; json.lisp --- JSON serialization of introspection information.
;;;;
;;;; Copyright (C) 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.introspection)

(defun encode-json-sequence (sequence &optional (stream json:*json-output*))
  (json::with-array (stream)
    (map nil (json::stream-array-member-encoder stream) sequence)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-members ((stream) &body body)
    (once-only (stream)
      `(json:with-object (,stream)
         (flet ((encode-member (name value)
                  (json:encode-object-member name value ,stream))
                (encode-member/sequence (name value)
                  (json:as-object-member (name stream)
                    (encode-json-sequence value stream))))
           (declare (dynamic-extent #'encode-member #'encode-member/sequence)
                    (ignorable #'encode-member #'encode-member/sequence))
           ,@body)))))

(defmethod json:encode-json ((object rsb.introspection::tracked-quantity)
                             &optional (stream json:*json-output*))
  (let+ (((&structure-r/o tracked-quantity- value history) object))
    (with-members (stream)
      (encode-member          "value"   value)
      (encode-member/sequence "history" history))))

(defmethod json:encode-json ((object participant-entry)
                             &optional (stream json:*json-output*))
  (let+ (((&structure-r/o entry- info children) object)
         ((&structure-r/o participant-info- kind id type scope) info))
    (with-members (stream)
      (encode-member          "kind"     (string-downcase kind))
      (encode-member          "id"       (princ-to-string id))
      (encode-member          "type"     type)
      (encode-member          "scope"    (scope-string scope))
      (encode-member/sequence "children" children))))

(defmethod json:encode-json ((object process-entry)
                             &optional (stream json:*json-output*))
  (let+ (((&accessors-r/o
           ((&structure-r/o
             process-info-
             process-id program-name commandline-arguments start-time
             executing-user rsb-version display-name
             state transports)
            entry-info)
           ((&structure-r/o timing-tracker- (latency %latency)) entry-%tracker)
           (participants introspection-participants/roots))
          object))
    (with-members (stream)
      (encode-member          "processId"            process-id)
      (encode-member          "programName"          program-name)
      (encode-member/sequence "commandlineArguments" commandline-arguments)
      (encode-member          "startTime"            (local-time:timestamp-to-unix
                                                      start-time))
      (encode-member          "executingUser"        executing-user)
      (encode-member          "rsbVersion"           rsb-version)
      (encode-member          "displayName"          display-name)
      (encode-member          "state"                (string-downcase state))
      (encode-member          "latency"              latency)
      (encode-member/sequence "participants"         participants)
      (encode-member/sequence "transports"           (mapcar #'princ-to-string
                                                             transports)))))

(defmethod json:encode-json ((object host-entry)
                             &optional (stream json:*json-output*))
  (let+ (((&accessors-r/o
           ((&structure-r/o host-info-
                            id hostname
                            machine-type machine-version
                            software-type software-version)
            entry-info)
           ((&structure-r/o timing-tracker-
                            (clock-offset %clock-offset)
                            (latency      %latency))
            entry-%tracker)
           (processes introspection-processes))
          object))
    (with-members (stream)
      (encode-member          "id"              id)
      (encode-member          "hostname"        hostname)
      (encode-member          "machineType"     machine-type)
      (encode-member          "machineVersion"  machine-version)
      (encode-member          "softwareType"    software-type)
      (encode-member          "softwareVersion" software-version)
      (encode-member          "clock-offset"    clock-offset)
      (encode-member          "latency"         latency)
      (encode-member/sequence "processes"       processes))))

(defmethod json:encode-json ((object remote-introspection-database)
                             &optional (stream json:*json-output*))
  (json:with-object (stream)
    (json:as-object-member ("hosts" stream)
      (encode-json-sequence (introspection-hosts object) stream))))

(defmethod json:encode-json ((object remote-introspection)
                             &optional (stream json:*json-output*))
  (json:encode-json (introspection-database object) stream))

(cl:in-package #:rsb.formatting.introspection)

;;; `style-json'

(defclass style-json (database-mixin)
  ()
  (:documentation
   "Serialize introspection information to JSON."))

(service-provider:register-provider/class
 'style :json :class 'style-json)

(defmethod detach ((participant style-json)))

;; Introspection event => ignore.
(defmethod rsb.ep:handle ((sink style-json) (data list)))

;; Dummy event => serialize snapshot to JSON and return false to
;; indicate that the program should be terminated.
(defmethod rsb.formatting:format-event ((event  t)
                                        (style  style-json)
                                        (target t)
                                        &key &allow-other-keys)
  (let+ (((&structure-r/o style- database) style))
    (with-database-lock (database)
      (json:encode-json database target)))
  nil)

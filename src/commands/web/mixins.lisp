;;;; mixins.lisp --- Mixin classes for web-related commands.
;;;;
;;;; Copyright (C) 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.tools.commands.web)

;;; `http-server-mixin'

(defclass http-server-mixin ()
  ((address       :initarg  :address
                  :type     (or null string)
                  :reader   command-address
                  :documentation
                  "Address on which the HTTP server should
                   listen.")
   (port          :initarg  :port
                  :type     (integer 0 65535)
                  :reader   command-port
                  :documentation
                  "Port on which the HTTP server should listen.")
   (document-root :type     (or null pathname)
                  :reader   command-document-root
                  :accessor command-%document-root
                  :initform nil
                  :documentation
                  "Directory from which static content such as HTML
                   and CSS files should be read.")
   (message-log   :initarg  :message-log
                  :type     (or boolean pathname)
                  :reader   command-message-log
                  :initform nil
                  :documentation
                  "Pathname of a file to which web server message
                   should be logged. `nil' disables message
                   logging. `t' logs to `*standard-output*'.")
   (access-log    :initarg  :access-log
                  :type     (or boolean pathname)
                  :reader   command-access-log
                  :initform nil
                  :documentation
                  "Pathname of a file to which web server accesses
                   should be logged. `nil' disables access
                   logging. `t' logs to `*standard-output*'.")
   (acceptor      :accessor command-%acceptor
                  :initform nil
                  :documentation
                  "A hunchentoot acceptor that dispatches requests to
                   handlers stored in the `handlers' slot.")
   (handlers      :type     list
                  :accessor command-handlers
                  :initform '()
                  :documentation
                  "A list of functions which accept
                   `hunchentoot:request' instances and process the
                   respective request."))
  (:default-initargs
   :address "localhost"
   :port    4444)
  (:documentation
   "This class is intended to be mixed into command classes that
    server data via HTTP."))

(defmethod shared-initialize :after
    ((instance   http-server-mixin)
     (slot-names t)
     &key
     (document-root nil document-root-supplied?))
  (when document-root-supplied?
    (setf (command-%document-root instance)
          (when document-root (pathname document-root)))))

(defmethod print-items:print-items append ((object http-server-mixin))
  (let ((endpoint (list (command-address object) (command-port object))))
    `((:marker   nil       " => "                                        ((:after :source-uris)))
      (:endpoint ,endpoint "~/rsb.tools.commands.web::print-server-url/" ((:after :marker))))))

(defmethod command-register-handler ((command http-server-mixin)
                                     (path    string)
                                     (handler t))
  (push (hunchentoot:create-prefix-dispatcher path handler)
        (acceptor-dispatch-table (command-%acceptor command))))

(defmethod command-execute ((command http-server-mixin) &key error-policy)
  (declare (ignore error-policy))
  (let+ (((&structure command- address port document-root
                      message-log access-log (acceptor %acceptor) handlers)
          command))
    (unwind-protect
         (progn
           ;; Construct acceptor.
           ;; TODO only for this acceptor, not globally, if possible
           (setf hunchentoot:*catch-errors-p*         nil
                 hunchentoot:*show-lisp-backtraces-p* t
                 hunchentoot:*show-lisp-errors-p*     t)
           (setf acceptor
                 (apply #'make-instance 'acceptor
                        :address       address
                        :port          port
                        :document-root document-root
                        (append
                         (case message-log
                           ((t))
                           ((nil) (list :message-log-destination nil))
                           (t     (list :message-log-destination message-log)))
                         (case access-log
                           ((t))
                           ((nil) (list :access-log-destination nil))
                           (t     (list :access-log-destination access-log))))))

           ;; Register endpoints.
           (command-register-handlers command)

           ;; Start acceptor.
           (log:info "~@<~A is starting acceptor ~A~@:>" command acceptor)
           (hunchentoot:start acceptor)
           (format t "Listening on ~/rsb.tools.commands.web::print-server-url/~%~
                      ~:[Not serving from filesystem~:;~:*Document ~
                      root is at ~S~]~%"
                   acceptor document-root)
           (log:info "~@<~A is serving ~S as ~
                      ~/rsb.tools.commands.web::print-server-url//~@:>"
                     command document-root acceptor)

           ;; Run
           (sleep most-positive-fixnum))

      ;; Clean up.
      (log:info "~@<~A is stopping acceptor ~A~@:>" command acceptor)
      (when acceptor
        (hunchentoot:stop acceptor))
      ;; Detach handlers.
      (mapc #'detach handlers))))

;;; `acceptor'

(defclass acceptor (hunchentoot:acceptor)
  ((dispatch-table :initarg  :dispatch-table
                   :type     list
                   :accessor acceptor-dispatch-table
                   :initform '()
                   :documentation
                   "Local dispatch table for the acceptor instance."))
  (:documentation
   "Specialized acceptor class with slot-stored dispatch table."))

(defmethod hunchentoot:handle-request ((acceptor acceptor)
                                       (request  t))
  (if-let ((handler (some (rcurry #'funcall request)
                          (acceptor-dispatch-table acceptor))))
    (funcall handler request)
    (call-next-method)))

(defmethod hunchentoot:acceptor-log-message
    ((acceptor acceptor) (log-level symbol) (format-string t)
     &rest format-arguments)
  (let+ ((logger (load-time-value (log:category) t))
         (level  (ecase log-level
                   (:error   log4cl:+log-level-error+)
                   (:warning log4cl:+log-level-warn+)
                   (:info    log4cl:+log-level-info+)))
         ((&flet write-message (stream)
            (declare (type stream stream))
            (apply #'format stream format-string format-arguments))))
    (declare (dynamic-extent #'write-message))
    (log4cl::log-with-logger logger level #'write-message *package*)))

;;; Utilities

(defun print-server-url (stream thing &optional at? colon?)
  (declare (ignore at? colon?))
  (multiple-value-call #'format stream "http://~:[*~;~:*~A~]:~D"
                       (etypecase thing
                         (hunchentoot:acceptor
                          (values (hunchentoot:acceptor-address thing)
                                  (hunchentoot:acceptor-port thing)))
                         (list
                          (values-list thing)))))

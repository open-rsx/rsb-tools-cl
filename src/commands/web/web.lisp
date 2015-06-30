;;;; web.lisp --- Serve system information via HTTP.
;;;;
;;;; Copyright (C) 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.tools.commands.web)

(defclass web (source-mixin
               response-timeout-mixin
               print-items:print-items-mixin)
  ((address          :initarg  :address
                     :type     (or null string)
                     :reader   command-address
                     :documentation
                     "Address on which the HTTP server should
                      listen.")
   (port             :initarg  :port
                     :type     (integer 0 65535)
                     :reader   command-port
                     :documentation
                     "Port on which the HTTP server should listen.")
   (static-directory :type     pathname
                     :reader   command-static-directory
                     :accessor command-%static-directory
                     :documentation
                     "Directory from which static content such as HTTP
                      and CSS files should be read.")
   (acceptor         :accessor command-%acceptor
                     :initform nil
                     :documentation
                     "A hunchentoot acceptor that dispatches requests
                      to handlers stored in the `handlers' slot.")
   (handlers         :type     list
                     :accessor command-handlers
                     :initform '()
                     :documentation
                     "A list of functions which accept
                      `hunchentoot:request' instances and process the
                      respective request."))
  (:default-initargs
   :address          "localhost"
   :port             4444
   :static-directory "static/")
  (:documentation
   "Serve information about an RSB system via HTTP.

    Introspection information is limited to hosts, processes and RSB
    participants reachable via the transports designated by URI* (zero
    or more URIs).

    When no URIs are supplied, the default transport configuration is
    used."))

(service-provider:register-provider/class
 'rsb.tools.commands::command :web :class 'web)

(defmethod shared-initialize :after
    ((instance   web)
     (slot-names t)
     &key
     (static-directory nil static-directory-supplied?))
  (when static-directory-supplied?
    (setf (command-%static-directory instance) (pathname static-directory))))

(defmethod print-items:print-items append ((object web))
  (let ((endpoint (list (command-address object) (command-port object))))
    `((:marker   nil       " => "                                        ((:after :source-uris)))
      (:endpoint ,endpoint "~/rsb.tools.commands.web::print-server-url/" ((:after :marker))))))

(defmethod command-register-handler ((command web)
                                     (path    string)
                                     (handler t))
  (push (hunchentoot:create-prefix-dispatcher
         (concatenate 'string "/" path) handler)
        (acceptor-dispatch-table (command-%acceptor command))))

(defmethod command-execute ((command web) &key error-policy)
  (let+ (((&structure
           command- uris response-timeout address port static-directory
           (acceptor %acceptor) handlers)
          command))
    (with-participant
        (database :remote-introspection rsb.introspection:+introspection-scope+
                  :receiver-uris    uris
                  :error-policy     error-policy
                  :response-timeout response-timeout)
      (unwind-protect
           (progn
             ;; Construct acceptor.
             ;; TODO only for this acceptor, not globally, if possible
             (setf hunchentoot:*catch-errors-p*         nil
                   hunchentoot:*show-lisp-backtraces-p* t
                   hunchentoot:*show-lisp-errors-p*     t)
             (setf acceptor (make-instance 'acceptor
                                           :address address
                                           :port    port))

             ;; Register endpoints.
             (setf (acceptor-dispatch-table acceptor)
                   (list (hunchentoot:create-folder-dispatcher-and-handler
                          "/static/" static-directory)))
             (mapc (curry #'apply #'command-register-handler command)
                   (list (list "introspection/json"
                               (make-instance 'introspection-json-handler
                                              :database database))))

             ;; Start acceptor.
             (log:info "~@<~A is starting acceptor ~A~@:>" command acceptor)
             (hunchentoot:start acceptor)
             (format t "Listening on ~/rsb.tools.commands.web::print-server-url/~%"
                     acceptor)
             (log:info "~@<~A is serving ~S as ~
                        ~/rsb.tools.commands.web::print-server-url//~A~@:>"
                       command static-directory acceptor "static")

             ;; Run
             (sleep most-positive-fixnum))

        ;; Clean up.
        (log:info "~@<~A is stopping acceptor ~A~@:>" command acceptor)
        (when acceptor
          (hunchentoot:stop acceptor))
        ;; Detach handlers.
        (mapc #'detach handlers)))))

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

(defmethod hunchentoot:handle-request ((acceptor acceptor) (request t))
  (if-let ((handler (some (rcurry #'funcall request)
                          (acceptor-dispatch-table acceptor))))
    (funcall handler request)
    (call-next-method)))

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

;;;; main.lisp --- Entry point of the call tool.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.tools.call)

(defun make-help-string (&key
                         (show :default))
  "Return a help that explains the commandline option interface."
  (with-output-to-string (stream)
    (format stream "Call METHOD of the server at SERVER-URI with ~
                    argument ARG.~@
                    ~@
                    ARG is parsed as string when surrounded with ~
                    double-quotes and as integer or float number when ~
                    consisting of digits without and with decimal ~
                    point respectively.~@
                    ~@
                    If ARG is the single character \"-\", the entire ~
                    \"contents\" of standard input (until end of file) ~
                    is read as a string and used as argument for the ~
                    method call.~@
                    ~@
                    If ARG is the empty string, i.e. the call ~
                    specification is of the form ~
                    SERVER-URI/METHOD(), the method is called without ~
                    argument.~@
                    ~@
                    SERVER-URI designates the root scope of the remote ~
                    server and the transport that should be used.~@
                    ~@
                    ")
    (with-abbreviation (stream :uri show)
      (progn
        (format stream "A SERVER-URI of the form~@
                        ~@
                        ")
        (print-uri-help stream :uri-var "SERVER-URI")))))

(defun make-examples-string (&key
                             (program-name #+does-not-work (progname) "call"))
  "Make and return a string containing usage examples of the program."
  (format nil
          "~2@T~A 'spread://localhost:4811/my/interface/method(5)'~@
           ~@
           Use the spread transport to call the method \"method\" of ~
           the server at \"/my/inferface\" passing it the integer ~
           argument \"5\". Note the quotes to prevent the shell from ~
           interpreting the \"(\" and \")\".~@
           ~@
           ~2@T~:*~A 'spread:/interface?name=4803/method(5)'~@
           ~@
           Like the previous example, but use the \"daemon name\" ~
           option of the Spread transport instead of specifying host ~
           and port. Note how URI options, being part of the server ~
           URI, are inserted between the URI path component and the ~
           method name.~@
           ~@
           ~2@T~:*~A '/my/interface/noarg()'~@
           ~@
           Use the default transport configuration to call the ~
           \"noarg\" method of the server at scope \"/my/interface\" ~
           without argument.~@
           ~@
           ~2@T~:*~A --no-wait '/remotecontrol/stop(\"now\")'~@
           ~@
           Use the default transport configuration to call the ~
           \"stop\" method of the server at scope \"/remotecontrol\" ~
           passing it the string argument \"now\". Do not wait for a ~
           result of the method call.~@
           ~@
           ~2@Tcat my-arg.txt | ~:*~A 'socket:/printer/print(-)'~@
           ~@
           Call the \"print\" method of the server at scope ~
           \"/printer\" using the socket transform (with its default ~
           configuration) using the content of the file \"my-arg.txt\" ~
           as argument of the call. This only works if the called ~
           method accepts an argument of type string.~@
           "
          program-name))

(defun update-synopsis (&key
                        (show :default))
  "Create and return a commandline option tree."
  (make-synopsis
   ;; Basic usage and specific options.
   :postfix "SERVER-URI/METHOD(ARG)"
   :item    (make-text :contents (make-help-string :show show))
   :item    (make-common-options :show show)
   :item    (make-error-handling-options :show show)
   :item    (defgroup (:header "Call options")
              (lispobj :long-name     "timeout"
                       :short-name    "t"
                       :typespec      'non-negative-real
                       :argument-name "SPEC"
                       :description
                       "If the result of the method call does not arrive within the amount of time specified by SPEC, consider the call to have failed and exit with non-zero status.")
              (flag    :long-name     "no-wait"
                       :short-name    "w"
                       :description
                       "Do not wait for the result of the method call. Immediately return with zero status without printing a result to standard output.")
              (stropt  :long-name       "style"
                       :short-name      "s"
                       :default-value   "payload :separator nil"
                       :argument-name   "SPEC"
                       :description
                       (make-style-help-string :show show)))
   ;; Append IDL options.
   :item    (make-idl-options)
   ;; Append RSB options.
   :item    (make-options
             :show? (or (eq show t)
                        (and (listp show) (member :rsb show))))
   ;; Append examples.
   :item    (defgroup (:header "Examples")
              (make-text :contents (make-examples-string)))))

(defun parse-argument (string)
  "Parse STRING as Lisp object treating the empty string specially."
  (cond
    ((emptyp string)
     rsb.converter:+no-value+)

    ((string= string "-")
     (with-output-to-string (stream)
       (copy-stream *standard-input* stream)))

    (t
     (let+ (((&values value consumed)
             (read-from-string string)))
       (unless (= consumed (length string))
         (error "~@<Junk at end of argument string: ~S.~@:>"
                (subseq string consumed)))
       value))))

(defun main ()
  "Entry point function of the cl-rsb-tools-call system."
  (update-synopsis)
  (setf *default-configuration* (options-from-default-sources))
  (process-commandline-options
   :version         (cl-rsb-tools-call-system:version/list :commit? t)
   :update-synopsis #'update-synopsis
   :return          (lambda () (return-from main)))
  (enable-swank-on-signal)

  ;; Validate commandline options.
  (unless (length= 1 (remainder))
    (error "~@<Supply call specification of the form ~
            SERVER-URI/METHOD(ARG).~@:>"))

  (with-logged-warnings
    ;; Load IDLs as specified on the commandline.
    (process-idl-options)

    ;; 1. Parse the method call specification
    ;; 2. Call the method and
    ;; 3. Potentially wait for a reply
    ;;    Format it
    (let+ ((error-policy (maybe-relay-to-thread
                          (process-error-handling-options)))
           (spec (first (remainder)))
           ((&values server-uri method arg)
            (ppcre:register-groups-bind (server-uri method arg)
                ("^([a-zA-Z0-9/:&?#=+;]*)/([a-zA-Z0-9]+)\\((.*)\\)$" spec)
              (values server-uri method (parse-argument arg))))
           (timeout (getopt :long-name "timeout"))
           (wait?   (not (getopt :long-name "no-wait")))
           (style   (let+ (((class &rest args)
                            (parse-instantiation-spec
                             (getopt :long-name "style"))))
                      (apply #'make-instance (find-style-class class)
                             args)))
           ((&flet call/raw (server)
              (cond
                ((not wait?)
                 (call server method arg :block? nil)
                 (values))
                ((not timeout)
                 (call server method arg
                       :return :event))
                (t
                 (handler-case
                     (call server method arg
                           :return  :event
                           :timeout timeout)

                   (bt:timeout (condition)
                     (declare (ignore condition))
                     (error "~@<Method call timed out after ~S ~
                             second~:P.~@:>"
                            timeout)))))))
           ((&flet call/translate (server)
              (let+ (((&values event) (call/raw server)))
                (cond
                  ((not event)
                   (values))
                  ((typep (event-data event) 'rsb.converter:no-value)
                   (values))
                  (t
                   event))))))

      (unless (and server-uri method arg)
        (error "~@<Parse error in call specification ~S.~@:>"
               spec))

      (when (and timeout (not wait?))
        (error "~@<Cannot specify timeout ~S in conjunction with
                no-wait.~@:>"
               timeout))

      (log1 :info "Using URI ~S method ~S arg ~A"
	    server-uri method arg)
      (with-interactive-interrupt-exit ()
        (with-error-policy (error-policy)
          (with-remote-server (server server-uri)
            (hooks:add-to-hook (participant-error-hook server)
                               error-policy) ; TODO(jmoringe, 2012-08-09): support in with-remote-server
            (when-let ((reply (multiple-value-list (call/translate server))))
              (format-event (first reply) style *standard-output*))))))))

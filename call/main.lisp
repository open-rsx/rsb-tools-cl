;;;; main.lisp --- Entry point of the call tool.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014, 2015 Jan Moringen
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
                    If ARG is the empty string, i.e. the call ~
                    specification is of the form ~
                    SERVER-URI/METHOD(), METHOD is called without ~
                    argument.~@
                    ~@
                    If ARG is one of the strings \"true\" and ~
                    \"false\", it is parsed as the corresponding ~
                    Boolean value.~@
                    ~@
                    ARG is parsed as string when surrounded with ~
                    double-quotes and as integer or float number when ~
                    consisting of digits without and with decimal ~
                    point respectively.~@
                    ~@
                    If ARG is the single character \"-\" or the string ~
                    \"-:binary\", the entire \"contents\" of standard ~
                    input (until end of file) is read as a string or ~
                    octet-vector respectively and used as argument for ~
                    the method call.~@
                    ~@
                    If ARG is of one the forms #P\"PATHNAME\", ~
                    #P\"PATHNAME\":ENCODING or #P\"PATHNAME\":binary, ~
                    the file designated by PATHNAME is read into a ~
                    string (optionally employing ENCODING) or ~
                    octet-vector and used as argument for the method ~
                    call.~@
                    ~@
                    Note that, when written as part of a shell ~
                    command, some of the above forms may require ~
                    protection from processing by the shell, usually ~
                    by surrounding the form in single quotes (').~@
                    ~@
                    SERVER-URI designates the root scope of the remote ~
                    server and the transport that should be used.~@
                    ~@
                    ")
    (with-abbreviation (stream :uri show)
      (format stream "A SERVER-URI of the form~@
                      ~@
                      ")
      (print-uri-help stream :uri-var "SERVER-URI"))))

(defun make-examples-string (&key (program-name "rsb call"))
  "Make and return a string containing usage examples of the program."
  (format nil
          "~2@T~A 'spread://localhost:4811/my/interface/method(5)'~@
           ~@
           Use the spread transport to call the method \"method\" of ~
           the server at \"/my/inferface\" passing it the integer ~
           argument \"5\".~@
           ~@
           Note the single quotes (') to prevent the shell from ~
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
           ~2@Tcat my-arg.txt | ~:*~A 'socket:/printer/print(-:binary)'~@
           ~2@T~:*~A 'socket:/printer/print(#P\"my-data.txt\")'~@
           ~2@T~:*~A 'socket:/printer/print(#P\"my-data.txt\":latin-1)'~@
           ~2@T~:*~A 'socket:/printer/print(#P\"my-data.txt\":binary)'~@
           ~@
           Call the \"print\" method of the server at scope ~
           \"/printer\" using the socket transform (with its default ~
           configuration) using the content of the file \"my-arg.txt\" ~
           as argument of the call. This only works if the called ~
           method accepts an argument of type string or octet-vector.~@

           Note the use of single quotes (') to prevent elements of ~
           the pathname #P\"my-data.txt\" from being processed by the ~
           shell.~@
           "
          program-name))

(defun update-synopsis (&key
                        (show         :default)
                        (program-name "rsb call"))
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
              (make-text :contents (make-examples-string
                                    :program-name program-name)))))

(defun main (program-pathname args)
  "Entry point function of the cl-rsb-tools-call system."
  (let ((program-name (concatenate
                       'string (namestring program-pathname) " call")))
    (update-synopsis :program-name program-name)
    (setf *configuration* (options-from-default-sources))
    (process-commandline-options
     :commandline     (list* program-name args)
     :version         (cl-rsb-tools-call-system:version/list :commit? t)
     :update-synopsis (curry #'update-synopsis :program-name program-name)
     :return          (lambda () (return-from main)))
    (enable-swank-on-signal))

  ;; Validate commandline options.
  (unless (length= 1 (remainder))
    (error "~@<Supply call specification of the form ~
            SERVER-URI/METHOD(ARG).~@:>"))

  (let ((error-policy (maybe-relay-to-thread
                       (process-error-handling-options))))
    (with-print-limits (*standard-output*)
      (with-logged-warnings
        (with-error-policy (error-policy)
          ;; Load IDLs as specified on the commandline.
          (process-idl-options)

          ;; 1. Parse the method call specification
          ;; 2. Call the method and
          ;; 3. Potentially wait for a reply
          ;;    Format it
          (let+ ((spec     (first (remainder)))
                 (timeout  (getopt :long-name "timeout"))
                 (no-wait? (getopt :long-name "no-wait"))
                 (style    (getopt :long-name "style"))
                 (command  (make-command :call
                                         :call-spec   spec
                                         :style-spec  style
                                         :timeout     timeout
                                         :no-wait?    no-wait?)))
            (with-interactive-interrupt-exit ()
              (command-execute command :error-policy error-policy))))))))

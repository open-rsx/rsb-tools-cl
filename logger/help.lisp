;;;; help.lisp --- Help text generation for the logger program.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsb.tools.logger)

(defun make-help-string (&key
                         (show :default))
  "Return a help that explains the commandline option interface."
  (with-output-to-string (stream)
    (format stream "Show events exchanged on the RSB channel ~
                    designated by URI. Events can be filtered and ~
                    displayed in several ways which can be controlled ~
                    using the --filter and --style options.~@
                    ~@
                    URI designates the channel for which events should ~
                    be received and logged and the transport that ~
                    should be used to attach to channel.~@
                    ~@
                    ")
    (with-abbreviation (stream :uri show)
      (format stream "A URI of the form~@
                      ~@
                      ~2@T")
      (print-uri-help stream))))

(defun make-filter-help-string (&key
                                (show :default))
  "Return a help string that explains how to specify filters and lists
the available filters."
  (with-output-to-string (stream)
    (format stream "Specify a filter that received events have to ~
                    match in order to be processed rather than ~
                    discarded. This option can be supplied multiple ~
                    times in which case events have to match all ~
                    specified filters. Each SPEC has to be of one of ~
                    the forms~@
                    ~@
                    ~2@TKIND | KIND SINGLE-VALUE | KIND KEY1 VALUE1 ~
                      KEY2 VALUE2 ...~@
                    ~@
                    where keys and values depend on KIND and may be ~
                    mandatory in some cases. Examples (note that the ~
                    single quotes have to be included only when used ~
                    within a shell):~@
                    ~@
                    ~2@T--filter 'origin \"EAEE2B00-AF4B-11E0-8930-001AA0342D7D\"'~@
                    ~2@T--filter 'regex \".*foo[0-9]+\"'~@
                    ~2@T--filter 'regex :regex \".*foo[0-9]+\"' (equivalent)~@
                    ~2@T-f 'xpath :xpath ~
                      \"node()/@foo\" :fallback-policy :do-not-match'~@
                    ~@
                    ")
    (with-abbreviation (stream :filters show)
      (format stream "The following filters are currently available:~@
                      ~@
                      ")
      (print-filter-help stream))))

(defun make-examples-string (&key
                             (program-name "logger"))
  "Make and return a string containing usage examples of the program."
  (format nil "~2@T~A~@
               ~@
               Use all enabled transports with their respective ~
               default configuration to access the bus. Receive and ~
               display all events exchanged on the entire bus (since ~
               the channel designated by the root scope, \"/\", is ~
               implicitly used).~@
               ~@
               ~2@T~:*~A spread://localhost:4811~@
               ~@

               Use the Spread daemon listening on port 4811 on ~
               localhost to connect to the bus. Since no scope is ~
               specified, receive and print all events exchanged on ~
               the entire bus.~@
               ~@
               ~2@T~:*~A -f 'regex :regex ~
                 \"^mypattern\" :fallback-policy :do-not-match' ~
                 --style detailed spread:/my/channel~@
               ~@
               Use the default configuration of the Spread transport ~
               to connect to the bus. Receive events on the channel ~
               designated by \"/my/channel\" (and sub-channels) the ~
               payloads of which match the regular expression ~
               \"^mypattern\". Display matching events using the ~
               \"detailed\" display style.~@
               "
          program-name))

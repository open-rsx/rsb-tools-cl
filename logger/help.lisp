;;;; help.lisp --- Help text generation for the logger program.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.tools.logger)

(defun make-help-string (&key
                         (show :default))
  "Return a help that explains the commandline option interface."
  (with-output-to-string (stream)
    (format stream "Show events exchanged on the RSB channel ~
                    designated by URIs. Events can be filtered and ~
                    displayed in several ways which can be controlled ~
                    using the --filter and --style options.~@
                    ~@
                    URIs designate the channel or channels for which ~
                    events should be received and logged and the ~
                    transport that should be used to attach to ~
                    channel(s). If no URIs are specified the root ~
                    scope / and default transports are assumed.~@
                    ~@
                    ")
    (with-abbreviation (stream :uri show)
      (format stream "Zero or more URIs of the form~@
                      ~@
                      ~2@T")
      (print-uri-help stream))))

(defun make-examples-string (&key (program-name "rsb logger"))
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
               ~@
               ~2@T~:*~A socket:/foo spread:/bar~@
               ~@
               Display events from two channels: the channel ~
               designated by \"/foo\", accessed via Spread transport, ~
               and the channel designated by \"/bar\", accessed via ~
               socket transport."
          program-name))

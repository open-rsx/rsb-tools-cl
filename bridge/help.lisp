;;;; help.lisp --- Help text generation for the bridge program.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.tools.bridge)

(defun make-help-string (&key
                         (show :default))
  (with-output-to-string (stream)
    (format stream "Forward events receivable in one part of an RSB ~
                    system to another part of the system.~@
                    ~@
                    Simple forwarding specifications describe from and ~
                    to which buses / scopes events should be forwarded ~
                    and can be constructed according to the following ~
                    grammar:~@
                    ~@
                    ~2@Tbridge-specification: ~
                      forwarding-specification (\";\" forwarding-specification)*~@
                    ~2@Tforwarding-specification: ~
                      unidirectional-forwarding-specification ~
                      | bidirectional-forwarding-specification~@
                    ~2@Tunidirectional-forwarding-specification: ~
                      (input-specification)+ ~
                      \"->\" ~
                      (filter)* ~
                      (output-specification)+~@
                    ~2@Tbidirectional-forwarding-specification: ~
                      (input-specification)+ ~
                       \"<->\" ~
                       (output-specification)+~@
                    ~2@Tfilter: FILTER~@
                    ~2@Tinput-specification: URI~@
                    ~2@Toutput-specification: URI~@
                    ~@
                    * Unidirectional forwarding, described by the ~
                      unidirectional-forwarding-specification ~
                      production, consists in forwarding events from ~
                      bus / scope(s) described by the ~
                      input-specifications on the left hand side of ~
                      the -> to the bus / scope(s) described by the ~
                      output-specification on the right hand side. ~
                      ~@
                    * Bidirectional forwarding, described by the ~
                      bidirectional-forwarding-specification ~
                      production, is like unidirectional forwarding ~
                      but also forwards events from the right hand ~
                      side to the left hand side. As a consequence, ~
                      filters are not supported.~@
                      ~@
                      ")
    (with-abbreviation (stream :uri show)
      (format stream "URIs are of the form~@
                      ~@
                      ~2@T")
      (print-uri-help stream))))

(defun make-examples-string (&key
                             (program-name "rsb bridge"))
  "Make and return a string containing usage examples of the program."
  (format nil "~2@T~A 'spread:/from -> socket:/to'~@
               ~@
               In the above example, the ~:*~A command is used to ~
               establish unidirectional forwarding from scope /from to ~
               scope /to within the bus designated by spread:.~@
               ~@
               Note the use of single quotes (') to prevent the shell ~
               from breaking up the forwarding specification into ~
               multiple arguments because of the whitespace in it.~@
               ~@
               ~2@T~:*~A 'socket://remotehost/ <-> socket://localhost/'~@
               ~@
               In the above example, the ~:*~A command is used to ~
               establish bidirectional forwarding affecting all events ~
               between remotehost and localhost.~@
               ~@
               Note the use of single quotes (') to prevent the shell ~
               from breaking up the forwarding specification into ~
               multiple arguments because of the whitespace in it."
          program-name))

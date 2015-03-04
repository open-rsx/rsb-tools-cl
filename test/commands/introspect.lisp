;;;; introspect.lisp --- Tests for the introspect command class.
;;;;
;;;; Copyright (C) 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.tools.commands.test)

(deftestsuite introspect-root (commands-root)
  ()
  (:documentation
   "Test suite for the `introspect' command."))

(addtest (introspect-root
          :documentation
          "Test construction of the `introspect' command.")
  construction

  (ensure-cases (initargs &optional expected)
      `(;; Some invalid cases.
        (()                        missing-required-initarg) ; :style[-spec] is missing
        ((:uris (,(puri:uri "/"))) missing-required-initarg) ; :style[-spec] is missing

        ;; These are Ok.
        ((:uris       (,(puri:uri "/"))
          :style-spec "object-tree"))
        ((:uris       (,(puri:uri "/"))
          :style      ,(rsb.formatting:make-style
                        :object-tree
                        :service 'rsb.formatting.introspection::style)))
        ((:uris       (,(puri:uri "/"))
          :style-spec "object-tree"))
        ((:uris       (,(puri:uri "/"))
          :style-spec "object-tree"
          :stream     ,*error-output*)))

    (let+ (((&flet do-it () (apply #'make-command :introspect initargs))))
      (case expected
        (missing-required-initarg
         (ensure-condition missing-required-initarg (do-it)))
        (t
         (ensure (typep (princ-to-string (do-it)) 'string)))))))

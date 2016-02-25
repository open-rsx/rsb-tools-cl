;;;; cl-rsb-formatting.asd --- Formatting functions for cl-rsb-based utilities.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:cl-rsb-formatting-system
  (:use
   #:cl
   #:asdf)

  (:export
   #:version/list
   #:version/string))

(cl:in-package #:cl-rsb-formatting-system)

;;; Version stuff

(defparameter +version-major+ 0
  "Major component of version number.")

(defparameter +version-minor+ 14
  "Minor component of version number.")

(let* ((version-file (merge-pathnames "version.sexp" *load-truename*))
       stream)
  (when (probe-file version-file)
    (setf stream (open version-file)))

  (defparameter +version-revision+ (if stream (read stream) 0)
    "Revision component of version number.")

  (defparameter +version-commit+ (when stream (read stream))
    "Commit component of version number.")

  (when stream (close stream)))

(defun version/list (&key
                     (revision? t)
                     commit?)
  "Return a version of the form (MAJOR MINOR [REVISION [COMMIT]])
where REVISION and COMMIT are optional.

REVISION? controls whether REVISION should be included. Default
behavior is to include REVISION.

COMMIT? controls whether COMMIT should be included. Default behavior
is to not include COMMIT."
  (append (list +version-major+ +version-minor+)
          (when revision? (list +version-revision+))
          (when (and commit? +version-commit+)
            (list +version-commit+))))

(defun version/string (&rest args
                       &key
                       revision?
                       commit?)
  "Return a version string of the form
\"MAJOR.MINOR[.REVISION[-.COMMIT]]\" where REVISION and COMMIT are
optional.

See `version/list' for details on keyword parameters."
  (declare (ignore revision? commit?))
  (format nil "~{~A.~A~^.~A~^-~A~}" (apply #'version/list args)))

;;; System definition

(defsystem :cl-rsb-formatting
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "GPLv3" ; see COPYING file for details.
  :description "This system provides some formatting functions for
RSB-related systems."
  :depends-on  (:alexandria
                :let-plus
                :more-conditions
                :cl-interpol
                (:version :log4cl                        "1.1.1")
                (:version :architecture.service-provider "0.1")

                (:version :utilities.binary-dump         "0.1")
                (:version :utilities.print-tree          "0.1")

                (:version :cl-rsb                        #.(version/string :revision? nil))
                (:version :rsb-protocol                  #.(version/string :revision? nil))  ; for payload-collection
                (:version :rsb-transport-socket          #.(version/string :revision? nil))  ; likewise
                (:version :rsb-introspection             #.(version/string :revision? nil)))
  :encoding    :utf-8
  :components  ((:module     "formatting-early"
                 :pathname   "src/formatting"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "types")
                              (:file       "conditions")
                              (:file       "variables")
                              (:file       "util")

                              (:file       "protocol")

                              (:file       "orders-of-magnitude")
                              (:file       "format-functions")
                              (:file       "dynamic-width")))

                (:module     "formatting-mixins"
                 :pathname   "src/formatting"
                 :depends-on ("formatting-early")
                 :components ((:file       "style-mixins")
                              (:file       "text-style-mixins")
                              (:file       "binary-style-mixins")))

                (:module     "formatting"
                 :pathname   "src/formatting"
                 :depends-on ("formatting-early"
                              "formatting-mixins")
                 :serial     t
                 :components (;; Column classes
                              (:file       "columns")

                              ;; Payload formatting classes
                              (:file       "payload-generic")

                              (:file       "rst-forward")
                              (:file       "payload-audio")
                              (:file       "payload-audio-wav")

                              ;; Event formatting style classes
                              (:file       "event-style-discard")
                              (:file       "event-style-meta-data")
                              (:file       "event-style-payload")
                              (:file       "event-style-detailed")
                              (:file       "event-style-compact")
                              (:file       "event-style-columns")
                              (:file       "event-style-programmable")

                              (:file       "payload-collection"))) ; Uses detailed style at load-time

                (:module     "formatting-introspection"
                 :pathname   "src/formatting/introspection"
                 :depends-on ("formatting-early")
                 :serial     t
                 :components ((:file       "package")
                              (:file       "print")
                              (:file       "styles"))))

  :in-order-to ((test-op (test-op :cl-rsb-formatting-test))))

;;; System definition for tests of the cl-rsb-formatting system

(defsystem :cl-rsb-formatting-test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "GPLv3" ; see COPYING file for details.
  :description "This system contains tests for the cl-rsb-formatting
system."
  :depends-on  (:cl-ppcre

                (:version :lift                  "1.7.1")

                (:version :cl-rsb-formatting     #.(version/string))
                (:version :cl-rsb-formatting-png #.(version/string))
                (:version :rsb-formatting-json   #.(version/string))

                (:version :cl-rsb-test           #.(version/string :revision? nil)))
  :encoding    :utf-8
  :components  ((:module     "formatting-early"
                 :pathname   "test/formatting"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "mock-column")

                              (:file       "protocol")

                              (:file       "orders-of-magnitude")
                              (:file       "dynamic-width")))

                (:module     "formatting"
                 :pathname   "test/formatting"
                 :depends-on ("formatting-early")
                 :components ((:file       "style-mixins")
                              (:file       "text-style-mixins")

                              (:file       "style-meta-data")
                              (:file       "style-detailed")
                              (:file       "style-compact")
                              (:file       "style-programmable")

                              (:file       "style-json")))

                (:module     "formatting-introspection"
                 :pathname   "test/formatting/introspection"
                 :depends-on ("formatting-early")
                 :serial     t
                 :components ((:file       "package")
                              (:file       "json")))))

(defmethod perform ((operation test-op)
                    (component (eql (find-system :cl-rsb-formatting-test))))
  (funcall (find-symbol "RUN-TESTS" :lift)
           :config (funcall (find-symbol "LIFT-RELATIVE-PATHNAME" :lift)
                            "lift-rsb-formatting.config")))

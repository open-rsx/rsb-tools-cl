;;;; rsb-tools-common.asd --- Common functions for cl-rsb-based utilities.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014, 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb-tools-common-system
  (:use
   #:cl
   #:asdf)

  (:export
   #:version/list
   #:version/string))

(cl:in-package #:rsb-tools-common-system)

;;; Version stuff

(defparameter +version-major+ 0
  "Major component of version number.")

(defparameter +version-minor+ 17
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

(defsystem :rsb-tools-common
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "GPLv3" ; see COPYING file for details.
  :description "This system provides some common functions for
RSB-related systems."
  :depends-on  (:let-plus
                (:version :log4cl "1.1.1")
                :uiop

                :esrap
                :cl-ppcre
                :architecture.builder-protocol
                :cl-protobuf

                :net.didierverna.clon

                (:version :cl-rsb   #.(version/string :revision? nil))
                (:version :rsb-clon #.(version/string :revision? nil)))
  :encoding    :utf-8
  :components  ((:module     "common"
                 :pathname   "src/common"
                 :serial     t
                 :components ((:file       "package")

                              (:file       "conditions")
                              (:file       "variables")

                              (:file       "error-handling")

                              (:file       "logging")
                              (:file       "debugger")
                              (:file       "interactive")
                              (:file       "help")
                              (:file       "protocol-buffer-payload")
                              (:file       "event")
                              (:file       "options")

                              (:file       "idl-loading")
                              (:file       "idl-loading-converter")
                              (:file       "idl-options"))))

  :in-order-to ((test-op (test-op :rsb-tools-common/test))))

;;; System definition for test of the rsb-tools-common system

(defsystem :rsb-tools-common/test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "GPLv3" ; see COPYING file for details.
  :description "This system contains tests for the rsb-tools-common
system."
  :depends-on  (:let-plus

                (:version :lift             "1.7.1")

                (:version :rsb-tools-common #.(version/string))

                (:version :rsb-protocol     #.(version/string :revision? nil)))
  :encoding    :utf-8
  :components  ((:module     "common"
                 :pathname   "test/common"
                 :serial     t
                 :components ((:file       "package")

                              (:file       "error-handling")

                              (:file       "protocol-buffer-payload")
                              (:file       "event")

                              (:file       "idl-loading")))))

(defmethod perform ((operation test-op)
                    (component (eql (find-system :rsb-tools-common/test))))
  (funcall (find-symbol "RUN-TESTS" :lift)
           :config (funcall (find-symbol "LIFT-RELATIVE-PATHNAME" :lift)
                            "lift-rsb-tools-common.config")))

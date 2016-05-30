;;;; rsb-tools-commands.asd --- System definition for the rsb-tools-commands system.
;;;;
;;;; Copyright (C) 2014, 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb-tools-commands-system
  (:use
   #:cl
   #:asdf)

  (:export
   #:version/list
   #:version/string))

(cl:in-package #:rsb-tools-commands-system)

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

   COMMIT? controls whether COMMIT should be included. Default
   behavior is to not include COMMIT."
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

;;; System definitions

(defsystem :rsb-tools-commands
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "GPLv3" ; see COPYING file for details.
  :description "Command protocol and basic commands provided by RSB tools."
  :depends-on  (:alexandria
                :let-plus
                (:version :more-conditions               "0.4")
                (:version :log4cl                        "1.1.1")
                (:version :architecture.service-provider "0.1")
                (:version :utilities.print-items         "0.1")

                (:version :cl-rsb                        #.(version/string :revision? nil))
                (:version :rsb-patterns-request-reply    #.(version/string :revision? nil))
                (:version :rsb-model                     #.(version/string :revision? nil)) ; for bridge

                (:version :rsb-tools-common              #.(version/string :revision? nil))
                (:version :cl-rsb-formatting             #.(version/string)))
  :encoding    :utf-8
  :components  ((:module     "commands"
                 :pathname   "src/commands"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "protocol")

                              (:file       "mixins")
                              (:file       "util")

                              (:file       "redump")

                              (:file       "info")
                              (:file       "logger")
                              (:file       "send")
                              (:file       "call")
                              (:file       "introspect")

                              (:file       "server")))

                (:module     "commands-bridge"
                 :pathname   "src/commands/bridge"
                 :depends-on ("commands")
                 :serial     t
                 :components ((:file       "package")
                              (:file       "conditions")
                              (:file       "model")
                              (:file       "grammar")
                              (:file       "participant")
                              (:file       "command"))))
  :in-order-to ((test-op (test-op :rsb-tools-commands-test))))

(defsystem :rsb-tools-commands-test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "GPLv3" ; see COPYING file for details.
  :description "Unit tests for rsb-tools-commands system."
  :depends-on  (:alexandria
                :let-plus

                (:version :lift                   "1.7.1")

                (:version :rsb-transport-socket   #.(version/string :revision? nil))

                (:version :rsb-tools-commands     #.(version/string))
                (:version :rsb-tools-commands-web #.(version/string))

                (:version :cl-rsb-test            #.(version/string :revision? nil)))
  :encoding    :utf-8
  :components  ((:module     "commands"
                 :pathname   "test/commands"
                 :serial     t
                 :components ((:file       "package")

                              (:file       "mixins")

                              (:file       "redump")

                              (:file       "info")
                              (:file       "logger")
                              (:file       "send")
                              (:file       "call")
                              (:file       "introspect")

                              (:file       "server")

                              (:file       "web")))

                (:module     "bridge"
                 :pathname   "test/commands/bridge"
                 :depends-on ("commands")
                 :serial     t
                 :components ((:file       "package")

                              (:file       "command")))))

(defmethod perform ((operation test-op)
                    (component (eql (find-system :rsb-tools-commands-test))))
  (funcall (find-symbol "RUN-TESTS" :lift)
           :config (funcall (find-symbol "LIFT-RELATIVE-PATHNAME" :lift)
                            "lift-rsb-tools-commands.config")))

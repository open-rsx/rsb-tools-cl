;;;; rsb-tools-logger.asd --- RSB Logging utility based rsb.
;;;;
;;;; Copyright (C) 2011-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb-tools-logger-system
  (:use
   #:cl
   #:asdf)

  (:export
   #:version/list
   #:version/string))

(cl:in-package #:rsb-tools-logger-system)

;;; Version stuff

(defparameter +version-major+ 0
  "Major component of version number.")

(defparameter +version-minor+ 19
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

(defsystem :rsb-tools-logger
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "GPLv3" ; see COPYING file for details.
  :description "A simple utility for receiving and displaying events
exchanged on a given RSB bus or channel."
  :depends-on  (:alexandria
                :let-plus
                :iterate
                (:version :lparallel                     "2.1.2")
                (:version :log4cl                        "1.1.1")

                :net.didierverna.clon

                (:version :rsb                           #.(version/string :revision? nil))

                (:version :rsb-tools-common              #.(version/string))
                (:version :rsb-stats                     #.(version/string))
                (:version :rsb-formatting-and-rsb-common #.(version/string))
                (:version :rsb-formatting-and-rsb-stats  #.(version/string))
                (:version :rsb-tools-commands            #.(version/string)))
  :encoding    :utf-8
  :components  ((:module     "logger"
                 :components ((:file       "package")
                              (:file       "help"
                               :depends-on ("package"))
                              (:file       "main"
                               :depends-on ("package" "help"))))))

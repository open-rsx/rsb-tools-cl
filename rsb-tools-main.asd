;;;; rsb-tools-main.asd --- System definition for main binary of rsb-tools.
;;;;
;;;; Copyright (C) 2011-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsb-tools-main-system
  (:use
   #:cl
   #:asdf)

  (:export
   #:version/list
   #:version/string))

(cl:in-package #:rsb-tools-main-system)

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

(defsystem :rsb-tools-main
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "GPLv3" ; see COPYING file for details.
  :description "Main program and dispatch function for all rsb tools."
  :depends-on  ((:version :asdf                 "3.1.5") ; for register-immutable-system

                (:version :rsb-introspection    #.(version/string :revision? nil))

                (:version :rsb-tools-info       #.(version/string))
                (:version :rsb-tools-logger     #.(version/string))
                (:version :rsb-tools-call       #.(version/string))
                (:version :rsb-tools-send       #.(version/string))
                (:version :rsb-tools-introspect #.(version/string))
                (:version :rsb-tools-web        #.(version/string))
                (:version :rsb-tools-bridge     #.(version/string))
                (:version :rsb-tools-server     #.(version/string))

                (:version :rsb-tools-commands   #.(version/string)))
  :encoding    :utf-8
  :components  ((:module     "main"
                 :components ((:file       "package")
                              (:file       "main"
                               :depends-on ("package")))))
  :build-operation program-op
  :build-pathname #-win32 "tools" #+win32 "tools.exe"
  :entry-point "rsb.tools.main:main")

(defmethod perform :before ((operation program-op)
                            (component (eql (find-system :rsb-tools-main))))
  (mapc (lambda (system)
          (uiop:symbol-call '#:asdf '#:register-immutable-system system))
        (already-loaded-systems)))

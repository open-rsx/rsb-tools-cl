;;; cl-rsb-tools-main.asd --- System definition for main binary of cl-rsb-tools.
;;
;; Copyright (C) 2011, 2012 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This Program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This Program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses>.

(cl:defpackage :cl-rsb-tools-main-system
  (:use
   :cl
   :asdf)

  (:export
   :version/list
   :version/string))

(cl:in-package :cl-rsb-tools-main-system)


;;; Version stuff
;;

(defparameter +version-major+ 0
  "Major component of version number.")

(defparameter +version-minor+ 8
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
;;

(defsystem :cl-rsb-tools-main
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "GPL3; see COPYING file for details."
  :description "Main program and dispatch function for all cl-rsb
tools."
  :depends-on  ((:version :cl-rsb-tools-info   #.(version/string))
		(:version :cl-rsb-tools-logger #.(version/string))
		(:version :cl-rsb-tools-call   #.(version/string))
		(:version :cl-rsb-tools-send   #.(version/string)))
  :components  ((:module     "main"
		 :components ((:file       "package")
			      (:file       "main"
			       :depends-on ("package"))))))

;;; cl-rsb-formatting.asd --- Formatting functions for cl-rsb-based utilities.
;;
;; Copyright (C) 2011, 2012, 2013 Jan Moringen
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

(cl:defpackage :cl-rsb-formatting-system
  (:use
   :cl
   :asdf)

  (:export
   :version/list
   :version/string))

(cl:in-package :cl-rsb-formatting-system)


;;; Version stuff
;;

(defparameter +version-major+ 0
  "Major component of version number.")

(defparameter +version-minor+ 10
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

(defsystem :cl-rsb-formatting
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "GPL3; see COPYING file for details."
  :description "This system provides some formatting functions for
RSB-related systems."
  :depends-on  (:alexandria
		:let-plus
		:more-conditions
		:cl-interpol

		(:version :cl-rsb #.(version/string :revision? nil)))
  :encoding    :utf-8
  :components  ((:module     "formatting-early"
		 :pathname   "formatting"
		 :serial     t
		 :components ((:file       "package")
			      (:file       "types")
			      (:file       "conditions")
			      (:file       "variables")
			      (:file       "util")

			      (:file       "protocol")

			      (:file       "format-functions")
			      (:file       "dynamic-width")))

		(:module     "formatting-mixins"
		 :pathname   "formatting"
		 :depends-on ("formatting-early")
		 :components ((:file       "width-mixin")
			      (:file       "name-mixin")
			      (:file       "counting-mixin")
			      (:file       "header-printing-mixin"
			       :depends-on ("counting-mixin"))
			      (:file       "columns-mixin"
			       :depends-on ("header-printing-mixin"
					    "width-mixin"))
			      (:file       "periodic-printing-mixin")
			      (:file       "delegating-mixin")
			      (:file       "sub-style-grouping-mixin"
			       :depends-on ("delegating-mixin"))
			      (:file       "sub-style-sorting-mixin"
			       :depends-on ("delegating-mixin"))
			      (:file       "separator-mixin")
			      (:file       "image-output-mixin")
			      (:file       "data-consistency-mixin")
			      (:file       "temporal-bounds-mixin")
			      (:file       "output-buffering-mixin")))

		(:module     "formatting"
		 :depends-on ("formatting-early"
			      "formatting-mixins")
		 :components ((:file       "payload")

			      ;; Column classes
			      (:file       "columns")

			      ;; Payload formatting classes
			      (:file       "payload-collection"
			       :depends-on ("event-style-detailed"))

			      (:file       "rst-forward")
			      (:file       "payload-audio"
			       :depends-on ("rst-forward"))
			      (:file       "payload-audio-wav"
			       :depends-on ("payload-audio"))

			      ;; Event formatting style classes
			      (:file       "event-style-discard")
			      (:file       "event-style-meta-data")
			      (:file       "event-style-payload")
			      (:file       "event-style-detailed"
			       :depends-on ("event-style-meta-data"))
			      (:file       "event-style-compact"
			       :depends-on ("columns"))
			      (:file       "event-style-columns"
			       :depends-on ("columns"))
			      (:file       "event-style-programmable"))))

  :in-order-to ((test-op (test-op :cl-rsb-formatting-test))))


;;; System definition for tests of the cl-rsb-formatting system
;;

(defsystem :cl-rsb-formatting-test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "GPL3; see COPYING file for details."
  :description "This system contains tests for the cl-rsb-formatting
system."
  :depends-on  (:cl-ppcre

		(:version :lift                  "1.7.1")

		(:version :cl-rsb-formatting     #.(version/string))
		(:version :cl-rsb-formatting-png #.(version/string)))
  :encoding    :utf-8
  :components  ((:module     "formatting-early"
		 :pathname   "test/formatting"
		 :serial     t
		 :components ((:file       "package")
			      (:file       "mock-column")))

		(:module     "formatting"
		 :pathname   "test/formatting"
		 :depends-on ("formatting-early")
		 :components ((:file       "width-mixin")
			      (:file       "delegating-mixin")
			      (:file       "columns-mixin")
			      (:file       "separator-mixin")

			      (:file       "style-meta-data")
			      (:file       "style-detailed")
			      (:file       "style-compact")
			      (:file       "style-programmable")))))

(defmethod perform ((operation test-op)
		    (component (eql (find-system :cl-rsb-formatting-test))))
  (funcall (find-symbol "RUN-TESTS" :lift)
	   :config (funcall (find-symbol "LIFT-RELATIVE-PATHNAME" :lift)
			    "lift-rsb-formatting.config")))

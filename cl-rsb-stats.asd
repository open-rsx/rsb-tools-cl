;;; cl-rsb-stats.asd --- Stats functions for cl-rsb-based utilities.
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

(cl:defpackage :cl-rsb-stats-system
  (:use
   :cl
   :asdf)

  (:export
   :version/list
   :version/string))

(cl:in-package :cl-rsb-stats-system)


;;; Version stuff
;;

(defconstant +version-major+ 0
  "Major component of version number.")

(defconstant +version-minor+ 8
  "Minor component of version number.")

(defconstant +version-revision+ 0
  "Revision component of version number.")

(defun version/list ()
  "Return a version of the form (MAJOR MINOR REVISION) "
  (list +version-major+ +version-minor+ +version-revision+))

(defun version/string ()
  "Return a version string of the form \"MAJOR.MINOR.REVISION\"."
  (format nil "~{~A.~A.~A~}" (version/list)))


;;; System definition
;;

(defsystem :cl-rsb-stats
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "GPL3; see COPYING file for details."
  :description "This system provides some stats functions for
RSB-related systems."
  :depends-on  (:let-plus
		:local-time

		(:version :cl-rsb #.(version/string)))
  :components  ((:module     "stats-early"
		 :pathname   "stats"
		 :serial     t
		 :components ((:file       "package")
			      (:file       "types")
			      (:file       "protocol")
			      (:file       "util")))

		(:module     "stats-mixins"
		 :pathname   "stats"
		 :depends-on ("stats-early")
		 :components ((:file       "named-mixin")
			      (:file       "collecting-mixin")
			      (:file       "histogram-mixin")
			      (:file       "extract-function-mixin")
			      (:file       "moments-mixin")
			      (:file       "all-time-mixin")
			      (:file       "reduction-mixin")
			      (:file       "rate-mixin")
			      (:file       "meta-data-mixin"
			       :depends-on ("named-mixin"))
			      (:file       "format-mixin")))

		(:module     "stats"
		 :depends-on ("stats-early" "stats-mixins")
		 :components ((:file       "quantities"))))

  :in-order-to ((test-op (test-op :cl-rsb-stats-test))))


;;; System definition for test of the cl-rsb-stats system
;;

(defsystem :cl-rsb-stats-test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "GPL3; see COPYING file for details."
  :description "This system contains tests for the cl-rsb-stats
system."
  :depends-on  ((:version :lift         "1.7.1")

		(:version :cl-rsb-stats #.(version/string)))
  :components  ((:module     "stats"
		 :pathname   "test/stats"
		 :components ((:file       "package")
			      (:file       "quantities"
			       :depends-on ("package"))))))

(defmethod perform ((operation test-op)
		    (component (eql (find-system :cl-rsb-stats-test))))
  (funcall (find-symbol "RUN-TESTS" :lift)
	   :config (funcall (find-symbol "LIFT-RELATIVE-PATHNAME" :lift)
			    "lift-rsb-stats.config")))

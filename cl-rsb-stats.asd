;;; cl-rsb-stats.asd --- Stats functions for cl-rsb-based utilities.
;;
;; Copyright (C) 2011 Jan Moringen
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

(defconstant +version-minor+ 5
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
  :depends-on  (:local-time

		(:version :cl-rsb #.(version/string)))
  :components  ((:module     "stats"
		 :components ((:file       "package")
			      (:file       "protocol"
			       :depends-on ("package"))

			      ;; Quantity mixin classes
			      (:file       "named-mixin"
			       :depends-on ("package" "protocol"))
			      (:file       "collecting-mixin"
			       :depends-on ("package" "protocol"))
			      (:file       "histogram-mixin"
			       :depends-on ("package" "protocol"))
			      (:file       "extract-function-mixin"
			       :depends-on ("package" "protocol"))
			      (:file       "moments-mixin"
			       :depends-on ("package" "protocol"))
			      (:file       "reduction-mixin"
			       :depends-on ("package" "protocol"))
			      (:file       "rate-mixin"
			       :depends-on ("package" "protocol"))

			      ;; Quantity classes
			      (:file       "quantities"
			       :depends-on ("package" "protocol"
					    "named-mixin"
					    "collecting-mixin"
					    "histogram-mixin"
					    "extract-function-mixin"
					    "moments-mixin"
					    "reduction-mixin"
					    "rate-mixin"))))))

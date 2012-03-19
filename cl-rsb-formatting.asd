;;; cl-rsb-formatting.asd --- Formatting functions for cl-rsb-based utilities.
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

(defconstant +version-major+ 0
  "Major component of version number.")

(defconstant +version-minor+ 7
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

(defsystem :cl-rsb-formatting
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "GPL3; see COPYING file for details."
  :description "This system provides some formatting functions for
RSB-related systems."
  :depends-on  (:let-plus
		:cl-interpol

		(:version :cl-rsb #.(version/string)))
  :components  ((:module     "formatting-early"
		 :pathname   "formatting"
		 :serial     t
		 :components ((:file       "package")
			      (:file       "types")
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
			      (:file       "separator-mixin")
			      (:file       "image-output-mixin")
			      (:file       "data-consistency-mixin")))

		(:module     "formatting"
		 :depends-on ("formatting-early"
			      "formatting-mixins")
			     :components (
			      (:file       "payload")

			      ;; Column classes
			      (:file       "columns")

			      ;; Payload formatting classes
			      (:file       "payload-collection"
			       :depends-on ("event-style-detailed"))

			      (:file       "rst-forward")
			      (:file       "payload-image-png"
			       :depends-on ("rst-forward"))

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


;;; System connection with cl-rsb-stats
;;

(defsystem-connection :cl-rsb-formatting-and-cl-rsb-stats
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "GPL3; see COPYING file for details."
  :description "This system connection adds a column-based event
formatting style, the columns of which are quantities defined in the
cl-rsb-stats system."
  :requires    (cl-rsb-formatting
		cl-rsb-stats)
  :components  ((:module     "formatting"
		 :components ((:file       "quantity-column")
			      (:file       "event-style-statistics"
			       :depends-on ("quantity-column"))))))


;;; System connection with cl-rsb-common
;;

(defsystem-connection :cl-rsb-formatting-and-cl-rsb-common
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "GPL3; see COPYING file for details."
  :description "This system connection adds formatting-related
handling of commandline options."
  :requires    (cl-rsb-formatting
		cl-rsb-common)
  :components  ((:file       "help"
		 :pathname   "formatting/help")))


;;; System definition for test of the cl-rsb-formatting system
;;

(defsystem :cl-rsb-formatting-test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "GPL3; see COPYING file for details."
  :description "This system contains tests for the cl-rsb-formatting
system."
  :depends-on  (:cl-rsb-formatting
		:cl-ppcre
		:lift)
  :components  ((:module     "formatting"
		 :pathname   "test/formatting"
		 :components ((:file       "package")
			      (:file       "mock-column"
			       :depends-on ("package"))

			      (:file       "width-mixin"
			       :depends-on ("package" "mock-column"))
			      (:file       "delegating-mixin"
			       :depends-on ("package" "mock-column"))
			      (:file       "columns-mixin"
			       :depends-on ("package" "mock-column"))

			      (:file       "separator-mixin"
			       :depends-on ("package"))
			      (:file       "style-meta-data"
			       :depends-on ("package"))
			      (:file       "style-detailed"
			       :depends-on ("package"))
			      (:file       "style-compact"
			       :depends-on ("package"))
			      (:file       "style-programmable"
			       :depends-on ("package")))))

  :in-order-to ((test-op (load-op :cl-rsb-formatting-test))))

(defmethod perform ((operation test-op)
		    (component (eql (find-system :cl-rsb-formatting-test))))
  (funcall (find-symbol "RUN-TESTS" :lift)
	   :config (funcall (find-symbol "LIFT-RELATIVE-PATHNAME" :lift)
			    "lift-rsb-formatting.config")))

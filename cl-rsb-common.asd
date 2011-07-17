;;; cl-rsb-common.asd --- Common functions for cl-rsb-based utilities.
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

(cl:defpackage :cl-rsb-common-system
  (:use
   :cl
   :asdf))

(cl:in-package :cl-rsb-common-system)

(defsystem :cl-rsb-common
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     "0.1.0"
  :license     "GPL3; see COPYING file for details."
  :description "This system provides some common functions for
RSB-related systems."
  :depends-on  (:com.dvlsoft.clon

		:cl-protobuf

		:cl-rsb)
  :components  ((:module     "common"
		 :components ((:file       "package")

			      (:file       "conditions"
			       :depends-on ("package"))

			      (:file       "filter-construction"
			       :depends-on ("package"))
			      (:file       "idl-loading"
			       :depends-on ("package" "conditions"))

			      (:file       "logging"
			       :depends-on ("package"))
			      (:file       "debugger"
			       :depends-on ("package"))
			      (:file       "interactive"
					   :depends-on ("package"))
			      (:file       "help"
					   :depends-on ("package"))
			      (:file       "options"
			       :depends-on ("package" "debugger" "help"))))))

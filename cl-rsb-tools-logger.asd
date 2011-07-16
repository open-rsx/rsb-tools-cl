;;; cl-rsb-tools-logger.asd --- RSB Logging utility based cl-rsb.
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

(cl:defpackage :cl-rsb-tools-logger-system
  (:use
   :cl
   :asdf))

(cl:in-package :cl-rsb-tools-logger-system)

(defsystem :cl-rsb-tools-logger
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     "0.1.0"
  :license     "GPL3; see COPYING file for details."
  :description "A simple utility for receiving and displaying events
exchanged on a given RSB bus or channel."
  :depends-on  (:alexandria
		:metabang-bind
		:iterate

		:yacc
		:com.dvlsoft.clon

		:cl-rsb
		:cl-rsb-common
		:cl-rsb-formatting)
  :components  ((:module     "logger"
		 :components ((:file       "package")

			      (:file       "main"
			       :depends-on ("package"))))))

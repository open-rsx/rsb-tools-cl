;;; rsb-formatting-and-rsb-common.asd --- Commandline options for formatting styles.
;;
;; Copyright (C) 2013 Jan Moringen
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

(load (merge-pathnames "cl-rsb-formatting.asd" *load-pathname*))

(cl:in-package #:cl-rsb-formatting-system)


;;; System definition
;;

(defsystem :rsb-formatting-and-rsb-common
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "GPL3; see COPYING file for details."
  :description "This system adds formatting-related handling of
commandline options."
  :depends-on  ((:version :cl-rsb-formatting #.(version/string))
		(:version :cl-rsb-common     #.(version/string)))
  :encoding    :utf-8
  :components  ((:file       "help"
		 :pathname   "formatting/help")))

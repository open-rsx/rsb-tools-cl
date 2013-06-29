;;; rsb-formatting-and-rsb-stats.asd --- Formatting styles based on cl-rsb-stats.
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

(defsystem :rsb-formatting-and-rsb-stats
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "GPL3; see COPYING file for details."
  :description "This system adds a column-based event formatting
style, the columns of which are quantities defined in the cl-rsb-stats
system."
  :depends-on  ((:version :cl-rsb-formatting #.(version/string))
		(:version :cl-rsb-stats      #.(version/string)))
  :encoding    :utf-8
  :components  ((:module     "formatting"
		 :components ((:file       "timeline")
			      (:file       "quantity-column")

			      (:file       "event-style-statistics"
			       :depends-on ("quantity-column"))
			      (:file       "event-style-monitor"
			       :depends-on ("quantity-column"))
			      (:file       "event-style-timeline"
			       :depends-on ("event-style-monitor"))))))

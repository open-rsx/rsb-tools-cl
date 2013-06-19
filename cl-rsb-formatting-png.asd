;;; cl-rsb-formatting-png.asd --- Formatting support for PNG payloads.
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

#.(progn
    (load (merge-pathnames "cl-rsb-formatting.asd" *load-truename*))
    (values))

(cl:in-package :cl-rsb-formatting-system)


;;; System definition
;;

(defsystem :cl-rsb-formatting-png
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "GPL3; see COPYING file for details."
  :description "This system provides some formatting of PNG payloads."
  :depends-on  (:alexandria
		:let-plus
		:iterate

		:zpng

		(:version :cl-rsb-formatting #.(version/string :revision? nil)))
  :encoding    :utf-8
  :components  ((:module     "formatting"
		 :components ((:file       "payload-image-png"))))

  :in-order-to ((test-op (test-op :cl-rsb-formatting-test))))

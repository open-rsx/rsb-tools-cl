;;; cl-rsb-formatting.asd --- 
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

(defpackage :cl-rsb-formatting-system
  (:use
   :cl
   :asdf)
  (:documentation
   "TODO(jmoringe): document"))

(in-package :cl-rsb-formatting-system)

(defsystem :cl-rsb-formatting
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     "0.1.0"
  :license     "LICENSE; see COPYING file for details."
  :description "This system provides some formatting functions for
RSB-related systems."
  :depends-on  (cl-rsb)
  :components  ((:module     "formatting"
		 :components ((:file       "package")
			      (:file       "util"
			       :depends-on ("package"))
			      
			      (:file       "protocol"
			       :depends-on ("package"))
			      (:file       "event"
			       :depends-on ("package" "util"))
			      (:file       "payload"
			       :depends-on ("package" "util"))))))

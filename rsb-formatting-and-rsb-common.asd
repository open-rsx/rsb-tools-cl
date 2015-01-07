;;;; rsb-formatting-and-rsb-common.asd --- Commandline options for formatting styles.
;;;;
;;;; Copyright (C) 2013, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(load (merge-pathnames "cl-rsb-formatting.asd" *load-pathname*))

(cl:in-package #:cl-rsb-formatting-system)

;;; System definition

(defsystem :rsb-formatting-and-rsb-common
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "GPLv3" ; see COPYING file for details.
  :description "This system adds formatting-related handling of
commandline options."
  :depends-on  ((:version :cl-rsb-formatting #.(version/string))
                (:version :cl-rsb-common     #.(version/string)))
  :encoding    :utf-8
  :components  ((:file       "help"
                 :pathname   "formatting/help")))

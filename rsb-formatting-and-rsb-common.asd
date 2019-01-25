;;;; rsb-formatting-and-rsb-common.asd --- Commandline options for formatting styles.
;;;;
;;;; Copyright (C) 2013-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(load (merge-pathnames "rsb-formatting.asd" *load-pathname*))

(cl:in-package #:rsb-formatting-system)

;;; System definition

(defsystem :rsb-formatting-and-rsb-common
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "GPLv3" ; see COPYING file for details.
  :description "This system adds formatting-related handling of
commandline options."
  :depends-on  ((:version :rsb-formatting   #.(version/string))
                (:version :rsb-tools-common #.(version/string)))
  :encoding    :utf-8
  :components  ((:file       "help"
                 :pathname   "src/formatting/help")))

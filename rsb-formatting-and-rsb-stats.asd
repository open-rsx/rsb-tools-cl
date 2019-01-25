;;;; rsb-formatting-and-rsb-stats.asd --- Formatting styles based on rsb-stats.
;;;;
;;;; Copyright (C) 2013-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(load (merge-pathnames "rsb-formatting.asd" *load-pathname*))

(cl:in-package #:rsb-formatting-system)

;;; System definition

(defsystem :rsb-formatting-and-rsb-stats
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "GPLv3" ; see COPYING file for details.
  :description "This system adds a column-based event formatting
style, the columns of which are quantities defined in the rsb-stats
system."
  :depends-on  ((:version :rsb-formatting #.(version/string))
                (:version :rsb-stats      #.(version/string)))
  :encoding    :utf-8
  :components  ((:module     "formatting"
                 :pathname   "src/formatting"
                 :serial     t
                 :components ((:file       "timeline")
                              (:file       "quantity-column")

                              (:file       "event-style-statistics")
                              (:file       "event-style-monitor")
                              (:file       "event-style-timeline")))))

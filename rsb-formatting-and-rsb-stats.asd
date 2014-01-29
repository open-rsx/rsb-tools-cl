;;;; rsb-formatting-and-rsb-stats.asd --- Formatting styles based on cl-rsb-stats.
;;;;
;;;; Copyright (C) 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(load (merge-pathnames "cl-rsb-formatting.asd" *load-pathname*))

(cl:in-package #:cl-rsb-formatting-system)

;;; System definition

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
                 :serial     t
                 :components ((:file       "timeline")
                              (:file       "quantity-column")

                              (:file       "event-style-statistics")
                              (:file       "event-style-monitor")
                              (:file       "event-style-timeline")))))

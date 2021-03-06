;;;; rsb-formatting-png.asd --- Formatting support for PNG payloads.
;;;;
;;;; Copyright (C) 2013-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

#.(progn
    (load (merge-pathnames "rsb-formatting.asd" *load-truename*))
    (values))

(cl:in-package #:rsb-formatting-system)

;;; System definition

(defsystem :rsb-formatting-png
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "GPLv3" ; see COPYING file for details.
  :description "This system provides some formatting of PNG payloads."
  :depends-on  (:alexandria
                :let-plus
                :iterate

                :zpng

                (:version :rsb-formatting #.(version/string :revision? nil)))
  :encoding    :utf-8
  :components  ((:module     "formatting"
                 :pathname   "src/formatting"
                 :components ((:file       "payload-image-png"))))

  :in-order-to ((test-op (test-op :rsb-formatting/test))))

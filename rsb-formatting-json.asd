;;;; rsb-formatting-json.asd --- Formatting support for JSON payloads.
;;;;
;;;; Copyright (C) 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

#.(progn
    (load (merge-pathnames "cl-rsb-formatting.asd" *load-truename*))
    (values))

(cl:in-package #:cl-rsb-formatting-system)

;;; System definitions

(defsystem :rsb-formatting-json
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "GPLv3" ; see COPYING file for details.
  :description "This system provides some formatting of JSON payloads."
  :depends-on  (:alexandria
                :let-plus

                :cl-json

                (:version :cl-rsb-formatting #.(version/string)))
  :encoding    :utf-8
  :components  ((:module     "formatting-introspection"
                 :pathname   "formatting/introspection"
                 :components ((:file       "json"))))

  :in-order-to ((test-op (test-op :cl-rsb-formatting-test))))

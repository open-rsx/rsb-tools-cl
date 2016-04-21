;;;; rsb-formatting-json.asd --- Formatting support for JSON payloads.
;;;;
;;;; Copyright (C) 2014, 2015, 2016 Jan Moringen
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

                (:version :architecture.builder-protocol.universal-builder "0.3")
                (:version :architecture.builder-protocol.json              "0.3")

                (:version :rsb-builder                                     #.(version/string :revision? nil))
                (:version :rsb-model-builder                               #.(version/string :revision? nil))

                (:version :cl-rsb-formatting                               #.(version/string)))
  :encoding    :utf-8
  :components  ((:module     "formatting"
                 :pathname   "src/formatting"
                 :components ((:file       "event-style-json")))

                (:module     "formatting-introspection"
                 :pathname   "src/formatting/introspection"
                 :depends-on ("formatting")
                 :components ((:file       "json"))))

  :in-order-to ((test-op (test-op :cl-rsb-formatting-test))))

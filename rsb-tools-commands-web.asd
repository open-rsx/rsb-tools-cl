;;;; rsb-tools-commands-web.asd --- Serve system information over HTTP.
;;;;
;;;; Copyright (C) 2014, 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

#.(cl:progn
   (cl:load (cl:merge-pathnames "rsb-tools-commands.asd" cl:*load-truename*))
   (cl:values))

(cl:in-package #:rsb-tools-commands-system)

;;; System definition

(defsystem :rsb-tools-commands-web
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "GPLv3" ; see COPYING file for details.
  :description "Serve introspection information over HTTP."
  :depends-on  (:alexandria
                :let-plus
                (:version :log4cl                              "1.1.1")

                (:version :architecture.builder-protocol.json  "0.4")
                (:version :architecture.builder-protocol.xpath "0.4")

                :hunchentoot

                (:version :cl-rsb                              #.(version/string :revision? nil))
                (:version :rsb-introspection                   #.(version/string :revision? nil))
                (:version :rsb-model-builder                   #.(version/string :revision? nil))

                (:version :rsb-tools-commands                  #.(version/string))

                (:version :rsb-formatting-json                 #.(version/string)))
  :encoding    :utf-8
  :components  ((:module     "web"
                 :pathname   "src/commands/web"
                 :serial     t
                 :components ((:file       "package")

                              (:file       "protocol")
                              (:file       "conditions")

                              (:file       "mixins")
                              (:file       "macros")
                              (:file       "command")
                              (:file       "introspection"))))
  :in-order-to ((test-op (test-op :rsb-tools-commands-web-test))))

(defsystem :rsb-tools-commands-web-test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "GPLv3" ; see COPYING file for details.
  :description "Unit tests for rsb-tools-commands system."
  :depends-on  (:alexandria
                :let-plus

                :drakma

                (:version :lift                    "1.7.1")

                (:version :rsb-tools-commands-web  #.(version/string))

                (:version :cl-rsb-test             #.(version/string :revision? nil))
                (:version :rsb-tools-commands-test #.(version/string :revision? nil)))
  :encoding    :utf-8
  :components  ((:module     "commands"
                 :pathname   "test/commands/web"
                 :serial     t
                 :components ((:file       "package")

                              (:file       "command")))))

(defmethod perform ((operation test-op)
                    (component (eql (find-system :rsb-tools-commands-web-test))))
  (funcall (find-symbol "RUN-TESTS" :lift)
           :config (funcall (find-symbol "LIFT-RELATIVE-PATHNAME" :lift)
                            "lift-rsb-tools-commands-web.config")))

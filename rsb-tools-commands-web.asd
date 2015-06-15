;;;; rsb-tools-commands-web.asd --- Serve system information over HTTP.
;;;;
;;;; Copyright (C) 2014, 2015 Jan Moringen
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
                (:version :log4cl              "1.1.1")

                :hunchentoot

                (:version :cl-rsb              #.(version/string :revision? nil))
                (:version :rsb-introspection   #.(version/string :revision? nil))

                (:version :rsb-tools-commands  #.(version/string))

                (:version :rsb-formatting-json #.(version/string)))
  :encoding    :utf-8
  :components  ((:module     "web"
                 :pathname   "src/commands/web"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "web")
                              (:file       "introspection")))))

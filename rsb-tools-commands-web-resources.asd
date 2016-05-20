;;;; rsb-tools-commands-web-resources.asd --- Serve resources from image over HTTP.
;;;;
;;;; Copyright (C) 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

#.(cl:progn
   (cl:load (cl:merge-pathnames "rsb-tools-commands.asd" cl:*load-truename*))
   (cl:values))

(cl:in-package #:rsb-tools-commands-system)

;;; System definition

(defsystem :rsb-tools-commands-web-resources
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "GPLv3" ; see COPYING file for details.
  :description "Serve web interface resources from image over HTTP."
  :depends-on  (:alexandria
                :let-plus
                (:version :log4cl                 "1.1.1")

                :hunchentoot
                :archive

                (:version :rsb-tools-commands     #.(version/string))
                (:version :rsb-tools-commands-web #.(version/string)))
  :encoding    :utf-8
  :components  ((:file       "src/commands/web/resources")))

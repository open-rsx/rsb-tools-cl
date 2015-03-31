;;;; cl-rsb-tools-introspect-graph.asd --- TODO.
;;;;
;;;; Copyright (C) 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

#. (cl:progn
     (cl:load (cl:merge-pathnames "cl-rsb-tools-introspect.asd" cl:*load-pathname*))
     (cl:values))

(cl:in-package #:cl-rsb-tools-introspect-system)

;;; System definition

(defsystem :cl-rsb-tools-introspect-graph
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "GPLv3" ; see COPYING file for details.
  :description "TODO"
  :depends-on  (:alexandria
                :let-plus

                :cl-dot

                (:version :cl-rsb                  #.(version/string :revision? nil))
                (:version :rsb-introspection       #.(version/string :revision? nil))

                (:version :cl-rsb-formatting       #.(version/string))
                (:version :cl-rsb-tools-introspect #.(version/string)))
  :encoding    :utf-8
  :components  ((:module     "introspect"
                 :components ((:file       "graph")))))

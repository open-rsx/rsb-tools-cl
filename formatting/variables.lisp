;;;; variables.lisp --- Variables used in the formatting module.
;;;;
;;;; Copyright (C) 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.formatting)

(declaim (special *textual-output-can-use-utf-8?*))

(defvar *textual-output-can-use-utf-8?* #-win32 t #+win32 nil
  "Controls whether UTF-8 (as opposed to ASCII) characters can be used
   in textual output.")

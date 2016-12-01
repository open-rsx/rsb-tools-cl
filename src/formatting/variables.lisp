;;;; variables.lisp --- Variables used in the formatting module.
;;;;
;;;; Copyright (C) 2012, 2013, 2014, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.formatting)

(defvar *output-unicode?* #-win32 t #+win32 nil
  "Controls whether the full Unicode range of characters (as opposed
   to ASCII) can be used in textual output.")

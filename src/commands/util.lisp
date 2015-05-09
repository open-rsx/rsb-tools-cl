;;;; util.lisp --- Utilities shared between commands.
;;;;
;;;; Copyright (C) 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.tools.commands)

(defun ensure-fallback-converter (&key (converters (default-converters)))
  (mapcar (lambda+ ((wire-type . converter))
            (cons wire-type
                  (if (and (listp converter)
                           (not (member :fundamental-null converter)))
                      (append converter '(:fundamental-null))
                      converter)))
          converters))

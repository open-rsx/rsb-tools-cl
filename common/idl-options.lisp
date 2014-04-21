;;;; idl-options.lisp --- IDL-related commandline options.
;;;;
;;;; Copyright (C) 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.common)

(defun existing-directory-or-lose (pathname)
  "Signal an error unless PATHNAME designates an existing directory."
  (if-let ((truename (probe-file pathname)))
    (when (or (pathname-name truename)
              (pathname-type truename))
      (error "~@<Not a directory: ~A.~@:>" truename))
    (error "~@<Directory does not exist: ~A.~@:>" pathname)))

(defun process-idl-options (&key
                            (purpose nil purpose-supplied?))
  "Process the options --idl-path and --load-idl by loading the
   specified IDL files."
  ;; Extend data definition source path.
  (iter (for paths next (getopt :long-name "idl-path"))
        (while paths)
        (iter (for path in paths)
              (existing-directory-or-lose path)
              (pushnew path pbf:*proto-load-path*)))

  ;; Load specified data definitions.
  (mapcar (apply #'rcurry #'load-idl :auto
                 (when purpose-supplied?
                   (list :purpose purpose)))
          (collect-option-values :long-name "load-idl"
                                 :transform #'identity)))

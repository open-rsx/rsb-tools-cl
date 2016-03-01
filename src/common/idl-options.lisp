;;;; idl-options.lisp --- IDL-related commandline options.
;;;;
;;;; Copyright (C) 2012, 2013, 2014, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.tools.common)

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
              (with-simple-restart (continue "~@<Skip path ~S~@:>" path)
                (existing-directory-or-lose path)
                (pushnew path pbf:*proto-load-path*))))

  ;; Load specified data definitions.
  (let ((sources (collect-option-values :long-name "load-idl"
                                        :transform #'identity)))
    (apply #'load-idl sources :auto
           (when purpose-supplied?
             (list :purpose purpose))))

  ;; If requested, enable on-demand IDL loading.
  (case (getopt :long-name "on-demand-idl-loading")
    (:blocking (setf *load-idl-on-demand?* t))))

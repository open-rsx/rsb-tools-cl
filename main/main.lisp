;;;; main.lisp --- Dispatch function of the main tools program.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.tools.main)

(defvar *name->entry-point*
  '(("info"           . rsb.tools.info:main)
    ("logger"         . rsb.tools.logger:main)
    ("call"           . rsb.tools.call:main)
    ("send"           . rsb.tools.send:main)
    ("introspect"     . rsb.tools.introspect:main)
    ("web"            . rsb.tools.web:main)
    ("bridge"         . rsb.tools.bridge:main)
    ("server"         . rsb.tools.server:main)

    ("bridge-service" . rsb.tools.bridge:main/service))
  "Stores a mapping from program names to entry point functions.")

(defun program-pathname->name (program-pathname)
  (apply #'concatenate 'string (pathname-name program-pathname)
         (when (pathname-type program-pathname)
           (list "." (pathname-type program-pathname)))))

(defun main ()
  "Entry point function of the main tools program."
  (make-synopsis)
  (let+ (((program-name &rest args) (net.didierverna.clon::cmdline))
         (program-pathname (pathname program-name)))
    (cond
      ;; If the program name does not correspond to an entry-point,
      ;; try to use the first commandline option as a sub-command.
      ((when-let* ((command (first args))
                   (entry   (assoc command *name->entry-point*
                                   :test #'string=)))
         (funcall (cdr entry) program-pathname (rest args))
         t))

      ;; If the program has been called with the "redump" commandline
      ;; option, dump into a new binary with the specified library
      ;; loading behavior and the specified core compression.
      ((string= "redump" (first args))
       (let+ (((&optional (name (program-pathname->name
                                 program-pathname))
                &rest local-args)
               (rest args))
              (static?   (when (member "static"   local-args :test #'string=)
                           t))
              (compress? (member "compress" local-args :test #'string=)))
         (command-execute (make-command :redump
                                        :output-file name
                                        :static?     static?
                                        :compression (when compress? 9)))))

      ;; Otherwise display information regarding entry points and
      ;; symbolic links and offer to create these automatically if
      ;; necessary.
      (t
       (format *error-output* "~@<Invoke this program as~
                               ~@:_~@:_~
                               ~5@T~A redump [FILENAME (compress|static)*]~
                               ~{~@:_  or ~{~A ~A~}~}~
                               ~@:_~@:_~
                               (not ~2:*~A).~@:>~%"
               program-pathname
               (mapcar (lambda (entry)
                         (list program-pathname (car entry)))
                       *name->entry-point*))))))

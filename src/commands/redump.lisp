;;;; redump.lisp --- Implementation of the redump command.
;;;;
;;;; Copyright (C) 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.tools.commands)

;;; Library loading behavior

(defun make-static ()
  "Hard-wire locations of foreign libraries."
  ;; Do not reload Spread library.
  #-win32
  (when (find-package '#:network.spread-system)
    (unless (uiop:symbol-call '#:network.spread-system '#:spread-library-pathname)
      (error "~@<Spread library pathname not provided (use ~
              SPREAD_LIBRARY environment variable).~@:>"))
    (uiop:symbol-call '#:network.spread '#:use-spread-library
                      :pathname (uiop:symbol-call '#:network.spread-system
                                                  '#:spread-library-pathname))
    (uiop:symbol-call '#:network.spread '#:disable-reload-spread-library)))

(defun make-dynamic ()
  "Enable dynamic search for and loading of foreign libraries."
  ;; Try to reload Spread library.
  #-win32
  (when (find-package '#:network.spread)
    (ignore-errors
      (uiop:symbol-call '#:network.spread '#:use-spread-library
                        :pathname nil))
    (uiop:symbol-call '#:network.spread '#:enable-reload-spread-library
                      :if-fails #'warn)))

;;; `redump' command class

(defclass redump ()
  ((output-file :type     pathname
                :reader   redump-output-file
                :writer   (setf redump-%output-file)
                :documentation
                "The file into which the current image should be
                 dumped.")
   (static?     :initarg  :static?
                :type     boolean
                :reader   redump-static?
                :initform nil
                :documentation
                "Should the binary be static in the sense of expecting
                 foreign libraries in the locations they have been
                 loaded from or should foreign libraries be searched
                 for on startup?")
   (compression :initarg  :compression
                :type     (or null (integer 0 9))
                :reader   redump-compression
                :initform nil
                :documentation
                "Should the dumped binary be compressed and if so, to
                 which extend? 0 is minimum compression, 9 is maximum
                 compression."))
  (:default-initargs
   :output-file (missing-required-initarg 'redump :output-file))
  (:documentation
   "Dump this program into a new binary.

    With the specified library loading behavior and the specified core
    compression."))

(service-provider:register-provider/class
 'command :redump :class 'redump)

(defmethod shared-initialize :after ((instance   redump)
                                     (slot-names t)
                                     &key
                                     (output-file nil output-file-supplied?))
  (when output-file-supplied?
    (setf (redump-%output-file instance)
          (etypecase output-file
            (string   (parse-namestring output-file))
            (pathname output-file)))))

(defmethod command-execute ((command redump) &key error-policy)
  (declare (ignore error-policy))

  (let+ (((&structure-r/o redump- output-file static? compression) command))
    ;; Change behavior for foreign libraries. Either hard-wire their
    ;; names into the dumped image ("static") or search for them on
    ;; image restart.
    (if static? (make-static) (make-dynamic))

    ;; Create new binary.
    #-sb-core-compression
    (when compression
      (warn "~@<Compression is not supported in this ~
             implementation~@:>"))
    (uiop:dump-image output-file
                     :executable t
                     #+sb-core-compression :compression
                     #+sb-core-compression compression)))

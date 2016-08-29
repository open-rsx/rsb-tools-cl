;;;; resources.lisp --- Serving resources from image over HTTP.
;;;;
;;;; Copyright (C) 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.tools.commands.web)

;;; Resource loading

(defun load-resource-files (files prefix)
  (let+ ((count 0)
         (size  0)
         ((&flet load-resource-file (pathname)
            (let ((name    (pathname (enough-namestring pathname prefix)))
                  (content (read-file-into-byte-vector pathname)))
              (incf count)
              (incf size (length content))
              (cons name content)))))
    (log:info "~@<Loading ~:D resource file~:P from ~S.~@:>"
              (length files) prefix)
    (prog1
        (mapcan (lambda (pathname)
                  (with-simple-restart (continue "Ignore the error")
                    (list (load-resource-file pathname))))
                files)
      (log:info "~@<Loaded ~:D resource file~:P, ~:D byte~:P from ~S.~@:>"
                count size prefix))))

(defun load-system-resource-files (system sub-directory)
  (let* ((base  (asdf:system-relative-pathname system sub-directory))
         (files (remove-if-not
                 #'pathname-name
                 (directory (merge-pathnames "**/*.*" base)))))
    (load-resource-files files base)))

(defparameter *resources*
  (let ((files (load-system-resource-files :rsb-tools-commands-web "resources/")))
    (alist-hash-table files :test #'equalp)))

;;; `resource-handler-mixin'

(defclass resource-handler-mixin ()
  ((resources :initarg  :resources
              :reader   handler-resources))
  (:default-initargs
   :resources (missing-required-initarg 'resource-handler-mixin :resources))
  (:documentation
   "Adds resource storage."))

(defmethod find-resource ((name t) (container resource-handler-mixin))
  (gethash name (handler-resources container)))

(defmethod map-resources ((function t) (container resource-handler-mixin))
  (maphash function (handler-resources container)))

(defmethod print-items:print-items append ((object resource-handler-mixin))
  `((:resource-count ,(hash-table-count (handler-resources object)) "(~:D)")))

;;; `resource-handler'

(defclass resource-handler (handler-mixin
                            resource-handler-mixin
                            print-items:print-items-mixin)
  ()
  (:metaclass closer-mop:funcallable-standard-class)
  (:documentation
   "Serves bundle resource files."))

(defmethod rsb.ep:handle ((sink resource-handler) (data hunchentoot:request))
  (let ((name (if (equal "/" (hunchentoot:script-name data))
                  #P"index.html"
                  (hunchentoot:request-pathname data))))
    (if-let ((content (find-resource name sink)))
      (progn
        (%static-resource-response (hunchentoot:mime-type name))
        content)
      (progn
        (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+)
        (hunchentoot:abort-request-handler)))))

;;; `source-archive-handler'

(defclass source-archive-handler (handler-mixin
                                  resource-handler-mixin
                                  print-items:print-items-mixin)
  ()
  (:metaclass closer-mop:funcallable-standard-class)
  (:documentation
   "Serves all bundled resource files in one archive."))

(defmethod rsb.ep:handle ((sink source-archive-handler)
                          (data hunchentoot:request))
  (%static-resource-response "application/x-tar")
  (let ((stream (hunchentoot:send-headers)))
    (archive:with-open-archive (archive stream
                                        :archive-type archive:tar-archive
                                        :direction    :output)
      (map-resources
       (lambda (name content)
         (let ((entry (make-instance 'archive::tar-entry
                                     :pathname name
                                     :mode     #o550
                                     :typeflag archive::+tar-regular-file+
                                     :uid      0
                                     :gid      0
                                     :size     (length content)
                                     :mtime    0)))
           (archive:write-entry-to-archive
            archive entry
            :stream (flexi-streams:make-in-memory-input-stream content))))
       sink)
      (archive:finalize-archive archive))))

;;; Activate the handlers

(defmethod command-make-handlers :around ((command web))
  ;; If COMMAND does not have a document root directory configured,
  ;; add one handler for builtin resources and another for download of
  ;; those in archive form.
  (if (command-document-root command)
      (call-next-method)
      (list*
       (cons ""
             (make-instance 'resource-handler :resources *resources*))
       (cons "/source.tar"
             (make-instance 'source-archive-handler :resources *resources*))
       (call-next-method))))

;;; Utilities

(defun %static-resource-response (&optional mime-type)
  (when mime-type
    (setf (hunchentoot:content-type*) mime-type))
  (setf (hunchentoot:header-out "Cache-Control")
        "no-transform,public,max-age=300,s-maxage=900"))

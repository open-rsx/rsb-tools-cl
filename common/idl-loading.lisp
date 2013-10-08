;;;; idl-loading.lisp --- Loading IDL files at runtime.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.common)

;;; IDL loading protocol

(defgeneric load-idl (source kind
                      &key
                      pathname
                      dependency-handler
                      &allow-other-keys)
  (:documentation
   "Try to load an IDL definition from SOURCE assuming source is of
the kind designated by KIND.

PATHNAME should be supplied, if SOURCE does not have an associated
pathname (e.g. not-file-based-stream) to allow but filename-based
heuristic should still be applied.

If supplied, DEPENDENCY-HANDLER has to be a function that resolves
dependencies. It is called when SOURCE contains references to entities
not specified within SOURCE."))

(defmethod load-idl :around  ((source t)
                              (kind   t)
                              &key &allow-other-keys)
  "Wrap error conditions in `failed-to-load-idl'."
  (handler-bind
      (((and error (not failed-to-load-idl))
        (curry #'failed-to-load-idl source)))
    (call-next-method)))

;;;

(defmethod load-idl ((source t) (kind t)
                     &key &allow-other-keys)
  "Signal an error if we no not know how load an IDL for the
combination SOURCE and KIND."
  (error "~@<~A is an unknown data definition type.~@:>"
         kind))

(defmethod load-idl ((source pathname) (kind (eql :file))
                     &rest args
                     &key &allow-other-keys)
  "Load an IDL definition from the file SOURCE. The IDL kind is
inferred form the file type of SOURCE."
  (log1 :info "Processing IDL file ~S" source)
  (apply #'load-idl
         source (make-keyword (string-upcase
                               (pathname-type source)))
         args))

(defmethod load-idl ((source pathname) (kind (eql :wild))
                     &rest args
                     &key &allow-other-keys)
  "Load IDL definitions from all files matching the wildcard pathname
SOURCE."
  (log1 :info "Processing IDL files matching ~S" source)
  (map 'list (lambda (file) (apply #'load-idl file :file args))
       (directory source)))

(defmethod load-idl ((source pathname) (kind (eql :auto))
                     &rest args
                     &key &allow-other-keys)
  "Load IDL definitions from the pathname SOURCE treating it as a wild
pathname, as directory or as a file depending on its properties."
  (cond
    ((eq (pathname-name source) :wild)
     (apply #'load-idl source :wild args))
    ((null (pathname-name source))
     (append
      (apply #'load-idl (merge-pathnames "*.proto" source) :wild args)
      (apply #'load-idl (merge-pathnames "*.protobin" source) :wild args)))
    (t
     (apply #'load-idl source :file args))))

(defmethod load-idl ((source string) (kind (eql :auto))
                     &rest args
                     &key &allow-other-keys)
  "Treat SOURCE as an URI if it contains a ':', treat it as a pathname
otherwise."
  (if (find #\: source)
      (apply #'load-idl (puri:parse-uri source) kind args)
      (apply #'load-idl (parse-namestring source) kind args)))

(defmethod load-idl ((source list) (kind t)
                     &rest args
                     &key &allow-other-keys)
  "Process all elements of SOURCE sequentially."
  (map 'list (lambda (source) (apply #'load-idl source kind args))
       source))

;;; Protocol buffer specific stuff

(defun process-descriptor (descriptor
                           &key
                           (emit '(:deserializer :extractor :offset)))
  "Emit a data-holder class and deserializer code for DESCRIPTOR."
  (log1 :info "Emitting data holder~@[ and ~(~{~A~^, ~}~)~] for ~A"
        emit descriptor)
  (prog1
      (pbb:emit descriptor :class)
    (map nil (curry #'pbb:emit descriptor) emit)))

(macrolet
    ((define-load-method (type kind func)
       `(defmethod load-idl ((source ,type) (kind (eql ,kind))
                             &rest args
                             &key
                             (purpose nil purpose-supplied?)
                             &allow-other-keys)
          (log1 :info "Parsing data definition from ~A" source)
          (apply #'process-descriptor
                 (apply #',func source (remove-from-plist args :purpose))
                 (when purpose-supplied?
                   (list :emit purpose))))))

  (define-load-method string   :proto    pbf:load/text)
  (define-load-method stream   :proto    pbf:load/text)
  (define-load-method pathname :proto    pbf:load/text)

  (define-load-method stream   :protobin pbf:load/binary)
  (define-load-method pathname :protobin pbf:load/binary))

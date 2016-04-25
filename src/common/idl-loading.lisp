;;;; idl-loading.lisp --- Loading IDL files at runtime.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.tools.common)

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
    dependencies. It is called when SOURCE contains references to
    entities not specified within SOURCE."))

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
  (error "~@<~A is an unknown data definition type.~@:>" kind))

(defmethod load-idl ((source pathname) (kind (eql :file))
                     &rest args
                     &key &allow-other-keys)
  "Load an IDL definition from the file SOURCE. The IDL kind is
   inferred form the file type of SOURCE."
  (log:info "~@<Processing IDL file ~S~@:>" source)
  (let ((kind (make-keyword (string-upcase (pathname-type source)))))
    (apply #'load-idl source kind args)))

(defmethod load-idl ((source pathname) (kind (eql :wild))
                     &rest args
                     &key &allow-other-keys)
  "Load IDL definitions from all files matching the wildcard pathname
   SOURCE."
  (log:info "~@<Processing IDL files matching ~S~@:>" source)
  (apply #'load-idl (directory source) :file args))

(defmethod load-idl ((source pathname) (kind (eql :auto))
                     &rest args
                     &key &allow-other-keys)
  "Load IDL definitions from the pathname SOURCE treating it as a wild
   pathname, as directory or as a file depending on its properties."
  (cond
    ((and (pathname-name source) (wild-pathname-p source))
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
  (with-sequence-progress (:load-idl source)
    (map 'list (progressing
                (lambda (source)
                  (with-simple-restart (continue "~@<Skip ~S~@:>" source)
                    (apply #'load-idl source kind args)))
                :load-idl "Loading ~A")
         source)))

;;; Protocol buffer specific stuff

(defvar *load-cache* (make-hash-table :test #'equal)
  "Cache for parsed descriptor objects that uses truenames as keys.")

(defvar *emit-caches* (make-hash-table :test #'eq)
  "Maps targets (i.e. :class, :serializer, etc.) to individual
   hash-table caches which use descriptor objects as keys.")

(defun process-descriptor (descriptor
                           &key
                           (emit '(:deserializer :extractor :offset)))
  "Emit a data-holder class and deserializer code for DESCRIPTOR."
  (log:info "~@<Emitting data holder~@[ and ~(~{~A~^, ~}~)~] for ~A~@:>"
            emit descriptor)
  (let+ (((&flet cache-arguments (which)
            `(:cache ,(ensure-gethash which *emit-caches*
                                      (make-hash-table :test #'eq))))))
    (prog1
        (pbb:emit descriptor `(:class ,@(cache-arguments :class)))
      (map nil (lambda (target)
                 (pbb:emit descriptor `(,target ,@(cache-arguments target))))
           emit))))

(macrolet
    ((define-load-method (type kind func)
       `(defmethod load-idl ((source ,type) (kind (eql ,kind))
                             &rest args
                             &key
                             (purpose nil purpose-supplied?)
                             &allow-other-keys)
          (log:info "~@<Parsing data definition from ~A~@:>" source)
          (apply #'process-descriptor
                 (let ((pbf:*cache* *load-cache*))
                   (apply #',func source (remove-from-plist args :purpose)))
                 (when purpose-supplied?
                   (list :emit purpose))))))

  (define-load-method string   :proto    pbf:load/text)
  (define-load-method stream   :proto    pbf:load/text)
  (define-load-method pathname :proto    pbf:load/text)

  (define-load-method stream   :protobin pbf:load/binary)
  (define-load-method pathname :protobin pbf:load/binary))

;;; Convenience functions

(defun find-and-load-idl (qualified-name kind &rest args &key purpose)
  "Find and load the definition of the IDL described by QUALIFIED-NAME
   and KIND.

   PURPOSE controls which methods are generated for the data type
   after loading the definition."
  (declare (ignore purpose))
  (let+ (((&flet parse-name (qualified-name)
            (let ((components (split-sequence #\. qualified-name
                                              :remove-empty-subseqs t)))
              (declare (type list components))
              (values (subseq components 0 (1- (length components)))
                      (lastcar components)))))
         ((&flet make-file-pathname (path package name)
            (reduce #'merge-pathnames
                    (list (make-pathname :name name :type (string-downcase kind))
                          (make-pathname :directory `(:relative ,@package))
                          path))))
         ((&flet find-definition (path package name)
            (probe-file (make-file-pathname path package name))))
         ((&values package name) (parse-name qualified-name))
         (file (or (some (rcurry #'find-definition package name)
                         pbf:*proto-load-path*)
                   (failed-to-load-idl
                    qualified-name
                    (make-condition
                     'simple-error
                     :format-control   "~@<Could not find data type ~
                                        definition for name ~S~@[ on ~
                                        search path ~{~S~^, ~}~].~@:>"
                     :format-arguments (list qualified-name
                                             pbf:*proto-load-path*))))))
    (apply #'load-idl file kind args)))

(defvar *load-idl-on-demand?* nil)

(defun ensure-idl-loaded (name &rest args &key purpose)
  (declare (ignore purpose))
  "Return descriptor of data type designated by NAME, loading it first
   if necessary."
  (or (pb:find-descriptor name :error? nil)
      (when *load-idl-on-demand?*
        (apply #'find-and-load-idl name :proto args)
        nil)
      (pb:find-descriptor name)))

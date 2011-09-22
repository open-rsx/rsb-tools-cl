;;; idl-loading.lisp --- Loading IDL files at runtime.
;;
;; Copyright (C) 2011 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This Program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This Program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses>.

(in-package :rsb.common)


;;; IDL loading protocol
;;

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


;;;
;;

(defmethod load-idl ((source t) (kind t)
		     &key &allow-other-keys)
  "Signal an error if we no not know how load an IDL for the
combination SOURCE and KIND."
  (failed-to-load-idl
   source
   (make-condition 'simple-error
		   :format-control  "~@<~A is an unknown data definition type.~@:>"
		   :format-arguments (list kind))))

(defmethod load-idl ((source pathname) (kind (eql :file))
		     &rest args
		     &key &allow-other-keys)
  "Load an IDL definition from the file SOURCE. The IDL kind is
inferred form the file type of SOURCE."
  (log1 :info "Processing IDL file ~S" source)
  (handler-bind
      ((error (curry #'failed-to-load-idl source)))
    (apply #'load-idl
	   source (make-keyword (string-upcase
				 (pathname-type source)))
	   args)))

(defmethod load-idl ((source pathname) (kind (eql :wild))
		     &rest args
		     &key &allow-other-keys)
  "Load IDL definitions from all files matching the wildcard pathname
SOURCE."
  (log1 :info "Processing IDL files matching ~S" source)
  (map 'list #'(lambda (file)
		 (apply #'load-idl file :file args))
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
  (map 'list #'(lambda (source) (apply #'load-idl source kind args))
       source))


;;; Protocol buffer specific stuff
;;

(defun process-descriptor (descriptor
			   &key
			   (emit '(:deserializer :extractor :offset)))
  "Emit a data-holder class and deserializer code for DESCRIPTOR."
  (log1 :info "Emitting data holder~@[ and ~(~{~A~^, ~}~)~] for ~A"
	emit descriptor)
  (prog1
      (pbb:emit descriptor :class)
    (map nil (curry #'pbb:emit descriptor) emit)))

(macrolet ((define-load-method (type kind func)
	     `(defmethod load-idl ((source ,type) (kind (eql ,kind))
				   &rest args
				   &key &allow-other-keys)
		(log1 :info "Parsing data definition from ~A" source)
		(process-descriptor (apply #',func source args)))))
  (define-load-method string   :proto    pbf:load/text)
  (define-load-method stream   :proto    pbf:load/text)
  (define-load-method pathname :proto    pbf:load/text)

  (define-load-method stream   :protobin pbf:load/binary)
  (define-load-method pathname :protobin pbf:load/binary))

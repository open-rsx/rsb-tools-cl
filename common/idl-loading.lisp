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

(defgeneric load-idl (source kind)
  (:documentation
   "Try to load an IDL definition from SOURCE assuming source is of
the kind designated by KIND."))


;;;
;;

(defmethod load-idl ((source t) (kind t))
  "Signal an error if we no not know how load an IDL for the
combination SOURCE and KIND."
  (failed-to-load-idl
   source
   (make-condition 'simple-error
		   :format-control  "~@<~A is an unknown data definition type.~@:>"
		   :format-arguments (list kind))))

(defmethod load-idl ((source pathname) (kind (eql :file)))
  "Load an IDL definition from the file SOURCE. The IDL kind is
inferred form the file type of SOURCE."
  (log1 :info "Processing IDL file ~S" source)
  (handler-bind
      ((error #'(lambda (condition)
		  (failed-to-load-idl source condition))))
    (load-idl source (make-keyword (string-upcase
				    (pathname-type source))))))

(defmethod load-idl ((source pathname) (kind (eql :wild)))
  "Load IDL definitions from all files matching the wildcard pathname
SOURCE."
  (log1 :info "Processing IDL files matching ~S" source)
  (map 'list (rcurry #'load-idl :file) (directory source)))

(defmethod load-idl ((source pathname) (kind (eql :auto)))
  "Load IDL definitions from the pathname SOURCE treating it as a wild
pathname, as directory or as a file depending on its properties."
  (cond
    ((eq (pathname-name source) :wild)
     (load-idl source :wild))
    ((null (pathname-name source))
     (append
      (load-idl (merge-pathnames "*.proto" source) :wild)
      (load-idl (merge-pathnames "*.protobin" source) :wild)))
    (t
     (load-idl source :file))))

(defmethod load-idl ((source string) (kind t))
  "Treat SOURCE as an URI if it contains a ':', treat it as a pathname
otherwise."
  (if (find #\: source)
      (load-idl (puri:parse-uri source) kind)
      (load-idl (parse-namestring source) kind)))

(defmethod load-idl ((source list) (kind t))
  "Process all elements of SOURCE sequentially."
  (map 'list (rcurry #'load-idl kind) source))


;;; Protocol buffer specific stuff
;;

(defun process-descriptor (descriptor)
  "Emit a data-holder class and deserializer code for DESCRIPTOR."
  (log1 :info "Emitting data holder and deserializer for ~A" descriptor)
  (prog1
      (pbb:emit descriptor :class)
    (pbb:emit descriptor :deserializer)))

(macrolet ((define-text-method (type)
	     `(defmethod load-idl ((source ,type) (kind (eql :proto)))
		(process-descriptor (pbf:load/text source)))))
  (define-text-method stream)
  (define-text-method pathname))

(macrolet ((define-binary-method (type)
	     `(defmethod load-idl ((source ,type) (kind (eql :protobin)))
		(process-descriptor (pbf:load/binary source)))))
  (define-binary-method stream)
  (define-binary-method pathname))

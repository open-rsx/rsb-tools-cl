;;; event-style-progammable.lisp --- A programmable formatting style.
;;
;; Copyright (C) 2011, 2012 Jan Moringen
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

(cl:in-package :rsb.formatting)


;;; Utility functions
;;

(defmacro with-interpol-syntax (() &body body)
  "Execute BODY with interpol syntax enabled."
  `(unwind-protect
	(progn
	  (interpol:enable-interpol-syntax)
	  ,@body)
     (interpol:disable-interpol-syntax)))

(defvar *style-programmable-default-bindings*
  `((sequence-number (event-sequence-number event))
    (id              (princ-to-string (event-id event)))
    (scope           (scope-string (event-scope event)))
    (origin          (event-origin event))
    (data            (event-data event))
    ,@(iter (for timestamp in '(create send receive deliver))
	    (collect `(,timestamp (timestamp event ,(make-keyword timestamp))))
	    (collect `(,(symbolicate timestamp "-UNIX")
			(local-time:timestamp-to-unix
			 (timestamp event ,(make-keyword timestamp)))))
	    (collect `(,(symbolicate timestamp "-UNIX-NSEC")
			(let ((ts (timestamp event ,(make-keyword timestamp))))
			  (+ (* (expt 10 9) (local-time:timestamp-to-unix ts))
			     (local-time:nsec-of ts))))))
    (causes          (map 'list #'event-id->uuid (event-causes event)))
    (skip-event      (throw 'skip-event nil)))
  "Default bindings available in instances of `style-programmable'.")

(defmethod find-style-class ((spec (eql :programmable)))
  (find-class 'style-programmable))

(defclass style-programmable ()
  ((template :type     string
	     :reader   style-template
	     :accessor %style-template
	     :documentation
	     "Stores the string template specifying the output format
of the style instance.")
   (bindings :initarg  :bindings
	     :type     list
	     :accessor style-bindings
	     :accessor %style-bindings
	     :initform *style-programmable-default-bindings*
	     :documentation
	     "Stores the bindings available in the output format
specification.")
   (lambda   :type     function
	     :accessor %style-lambda
	     :documentation
	     "Stores the compiled output formatting function for the
style instance."))
  (:default-initargs
   :template (missing-required-initarg 'style-programmable :template))
  (:documentation
   "This formatting style produces its output by applying a template
specification to individual events. In the template specification,
event properties can be accessed using a syntax of the form
${PROPERTY} or @{PROPERTY} for \"direct\" expansion and \"spliced\"
expansion respectively. In addition, interpolations like named unicode
characters etc. as described in http://weitz.de/cl-interpol/ are
supported.

By default, the following PROPERTY names are available:
"))

(setf (documentation 'style-programmable 'type)
      (format nil "~A~{+ ~(~A~)~^~%~}"
	      (documentation 'style-programmable 'type)
	      (map 'list #'first *style-programmable-default-bindings*)))

(defmethod shared-initialize :after ((instance   style-programmable)
                                     (slot-names t)
                                     &key
				     (template nil template-supplied?)
				     (bindings nil bindings-supplied?))
  (when template-supplied?
    (check-type template template-designator))
  (when bindings-supplied?
    (check-type bindings list "a list of items of the form (SYMBOL FORM)"))

  (cond
    ((and bindings-supplied? template-supplied?)
     (setf (%style-bindings instance) bindings))
    (bindings-supplied?
     (setf (style-bindings instance) bindings)))
  (when template-supplied?
    (setf (style-template instance) template)))

(defmethod (setf style-template) ((new-value pathname)
				  (style     style-programmable))
  (setf (style-template style)
	(handler-bind
	    ((error (lambda (condition)
		      (error "~@<Failed to read template from file ~S: ~A~:@>"
			     new-value condition))))
	  (read-file-into-string new-value)))
  new-value)

(defmethod (setf style-template) ((new-value string)
				  (style     style-programmable))
  (setf (%style-template style) new-value))

(defmethod (setf style-template) :after ((new-value string)
					 (style     style-programmable))
  (%recompile style))

(defmethod (setf style-bindings) :after ((new-value list)
					 (style     style-programmable))
  (%recompile style))

(defmethod format-event ((event  event)
			 (style  style-programmable)
			 (stream stream)
			 &key &allow-other-keys)
  (handler-bind
      ((error (lambda (condition)
		(error "~@<Failed to format event ~A using specified ~
bindings~_~{~2T~{~16A -> ~A~_~}~}and template~_~2T~S~_: ~A~@:>"
		       event
		       (style-bindings style) (style-template style)
		       condition))))
      (funcall (%style-lambda style) event stream)))

(defmethod print-object ((object style-programmable) stream)
  (let+ (((&accessors-r/o (template style-template)
			  (bindings style-bindings)) object)
	 (length (length template)))
   (print-unreadable-object (object stream :type t :identity t)
     (format stream "\"~A~:[~;…~]\"~:[~; (~D)~]"
	     (subseq template 0 (min 8 length)) (> length 8)
	     (not (eq bindings *style-programmable-default-bindings*))
	     (length bindings)))))


;;; (Re-)Compilation
;;

(defgeneric %recompile (style)
  (:documentation
   "Recompile the formatting function for the current template and
bindings of STYLE."))

(defgeneric compile-template (style template
			      &key
			      bindings)
  (:documentation
   "Compile TEMPLATE using BINDINGS and return the resulting compiled
function."))

(defmethod %recompile ((style style-programmable))
  (setf (%style-lambda style)
	(compile-template style (style-template style))))

(defmethod compile-template ((style    style-programmable)
			     (template string)
			     &key
			     (bindings (style-bindings style)))
  (log1 :info style "Compiling template ~S" template)
  (let+ ((form (handler-bind
		   ((error (lambda (condition)
			     (error "~@<Failed to read template string ~S: ~A~@:>"
				    template condition))))
		 (let ((*package* #.*package*))
		   (with-interpol-syntax ()
		     (read-from-string
		      (format nil "#?\"~A\"" template))))))
	 ((&values function nil failed?)
	  (compile
	   nil
	   `(lambda (event stream)
	      (declare (ignorable event))
	      (format stream (symbol-macrolet (,@bindings)
			       ,form))))))
    (when failed?
      (error "~@<Failed to compile template ~S.~@:>"
	     template))
    function))

;; Local Variables:
;; coding: utf-8
;; End:

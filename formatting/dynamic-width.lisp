;;; dynamic-width.lisp --- Tools for dynamic-width meta-styles.
;;
;; Copyright (C) 2012 Jan Moringen
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

(defun number-of-columns (min &optional max)
  "Return a predicate that matches if the current stream line width is
between MIN and MAX columns."
  #'(lambda (event)
      (declare (ignore event))

      (and (not (columns-exhausted? min))
	   (or (not max) (columns-exhausted? max)))))

(defmacro define-dynamic-width-style
    ((name
      &key
      (spec       (make-keyword name))
      (class-name (symbolicate :style "-" name))
      superclasses)
     &body specs-and-doc)
  "Define an event formatting style named NAME that dispatches event
formatting to sub-styles based on the available horizontal room."
  (let+ (((&values specs nil documentation)
	  (parse-body specs-and-doc :documentation t))
	 ((&flet+ make-sub-style (((min &optional max) style))
	    `(cons (number-of-columns ,min ,@(when max `(,max)))
		   ,style))))
    `(progn
       (defmethod find-style-class ((spec (eql ,spec)))
	 (find-class ',class-name))

       (defclass ,class-name (delegating-mixin
			      ,@superclasses)
	 ()
	 (:default-initargs
	  :sub-styles (list ,@(map 'list #'make-sub-style specs)))
	 (:documentation
	  ,(or documentation
	       (format nil "~@(~A~) meta-style that dispatches to one ~
of the ~:*~(~A~) styles based on available horizontal room."
		       name)))))))

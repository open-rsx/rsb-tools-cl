;;; filter-construction.lisp --- Construct filter instances from textual specs.
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

(in-package :rsb.tools.logger)


;;; Filter spec parsing and filter construction
;;

(defun parse-filter-spec (string)
  "Parse STRING as a filter specification of one of the forms

  KIND KEY1 VALUE1 KEY2 VALUE2 ...

and  

  KIND VALUE1

and return the result as a list."
  (maybe-expand-filter-spec
   (with-input-from-string (stream string)
     (iter (for token in-stream stream)
	   (collect token)))))

(defun make-filter (spec)
  "Construct and return a list of filters according to SPEC."
  (apply #'rsb.filter:filter spec))


;;; Utility functions
;;

(defun simple-filter-spec? (spec)
  "Return non-nil if SPEC is a \"simple\" filter specification of the
form (KIND SOLE-ARGUMENT)."
  (and (length= 2 spec) (keywordp (first spec))))

(defun maybe-expand-filter-spec (spec)
  "Expand SPEC into a full filter specification if it is a simple
filter specification. Otherwise, just return SPEC."
  (if (simple-filter-spec? spec)
      (cons (first spec) spec)
      spec))

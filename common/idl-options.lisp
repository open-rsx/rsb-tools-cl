;;; idl-options.lisp --- IDL-related commandline options.
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

(cl:in-package :rsb.common)

(defun existing-directory-or-lose (pathname)
  "Signal an error unless PATHNAME designates an existing directory."
  (if-let ((truename (probe-file pathname)))
    (when (or (pathname-name truename)
	      (pathname-type truename))
      (error "~@<Not a directory: ~A.~@:>" truename))
    (error "~@<Directory does not exist: ~A.~@:>" pathname)))

(defun process-idl-options ()
  "Process the options --idl-path and --load-idl by loading the
specified IDL files."
  ;; Extend data definition source path.
  (iter (for paths next (getopt :long-name "idl-path"))
	(while paths)
	(iter (for path in paths)
	      (existing-directory-or-lose path)
	      (pushnew path pbf:*proto-load-path*)))

  ;; Load specified data definitions.
  (map 'list (rcurry #'load-idl :auto)
       (collect-option-values :long-name "load-idl"
			      :transform #'identity)))

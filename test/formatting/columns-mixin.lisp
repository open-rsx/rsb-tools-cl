;;; columns-mixin.lisp --- Unit tests for the columns-mixin class.
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

(cl:in-package :rsb.formatting.test)

(deftestsuite columns-mixin-root (formatting-root)
  ()
  (:documentation
   "Test suite for the `columns-mixin' mixin class."))

(addtest (columns-mixin-root
          :documentation
	  "Test construction of the `columns-mixin' class.")
  construction

  (ensure-cases (args expected)
      '(((:columns (5))                     :error)
	((:columns (()))                    :error)
	((:columns (:no-such-column-class)) :error)

	(()                                 nil)
	((:columns   ())                    nil)
	((:separator "=")                   nil)
	((:columns   (:mock))               (mock-column))
	((:columns   ((:mock :width 2)))    (mock-column)))

    (if (eq expected :error)
	(ensure-condition 'error
	  (apply #'make-instance 'columns-mixin args))
	(let ((instance (apply #'make-instance 'columns-mixin args)))
	  (ensure-same (map 'list #'class-of (style-columns instance))
		       (map 'list #'find-class expected)
		       :test #'equal)))))

(addtest (columns-mixin-root
          :documentation
	  "Test the method on `format-event' for `columns-mixin' which
should format events according to the column specification of the
style.")
  format-event

  (ensure-style-cases (columns-mixin)
    ;; no columns, no events => no output
    '(()
      ()
      "")

    ;; events, but no columns => no output
    '(()
      ("/foo/bar")
      "")

    ;; single column => separator should not be printed
    '((:columns ((:mock :width 16 :alignment :left)) :separator "!")
      ("/foo/bar/")
      "/foo/bar/       ")

    ;; two columns => separator should be printed
    '((:columns   ((:mock :width 16 :alignment :right)
		   (:mock :width 8  :alignment :left))
       :separator "!")
      ("/foo/bar/")
      "       /foo/bar/!/foo/ba…")))

;;; style-programmable.lisp --- Unit tests for the programmable formatting style.
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

(in-package :rsb.formatting.test)

(deftestsuite style-programmable-root (formatting-root)
  ()
  (:documentation
   "Unit tests for the `style-programmable' formatting style class."))

(addtest (style-programmable-root
          :documentation
	  "Test constructing `style-programmable' instances.")
  construct

  (ensure-cases (args expected)
      '((()                   :error)
	((:template 5)        :error)
	((:template "${")     :error)
	((:template "${bla}") "${bla}"))

    (if (eq expected :error)
	(ensure-condition 'error
	  (apply #'make-instance 'style-programmable args))
	(let ((style (apply #'make-instance 'style-programmable
			    args)))
	  (ensure-same (style-template style) (getf args :template)
		       :test #'string=)))))

(addtest (style-programmable-root
          :documentation
	  "Test some simple cases of formatting evetns using methods
on `format-event' for `style-programmable'.")
  smoke

  (ensure-style-cases (style-programmable)
    '((:template "")
      ()
      "")

    `((:template "")
      (,(make-event "/foo" "bar"))
      "")))

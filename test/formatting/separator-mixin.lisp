;;; separator-mixin.lisp --- Unit tests for the `separator-mixin' class.
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

(cl:in-package :rsb.formatting.test)


;;; Mock formatter class
;;

(defclass mock-style-separator (separator-mixin)
  ()
  (:documentation
   "Unit tests for the `separator-mixin' formatting style class."))

(defmethod format-event ((event  event)
			 (style  mock-style-separator)
			 (stream stream)
			 &key &allow-other-keys)
  (princ #\* stream))


;;; Test suite
;;

(deftestsuite separator-mixin-root (formatting-root)
  ()
  (:documentation
   "Unit tests for the `separator-mixin' class."))

(addtest (separator-mixin-root
          :documentation
	  "Test constructing `separator-mixin' instances.")
  construction

  (ensure-cases (args expected)
      '(((:separator 5)            :error)
	((:separator (:hrule #\-)) :error)
	((:separator (:rule))      :error)

	((:separator "bla")        nil)
	((:separator ("a" #\-))    nil)
	((:separator (:rule #\=))  nil))

    (when (eq expected :error)
      (ensure-condition 'error
	(apply #'make-instance 'separator-mixin args)))))

(addtest (separator-mixin-root
          :documentation
	  "Test some simple cases of formatting events using methods
on `format-event' for `separator-mixin'.")
  smoke

  (ensure-style-cases (mock-style-separator)
    `((:separator nil)
      (,(make-event "/a" "b") ,(make-event "/c" "d"))
      "\\*\\*")

    `((:separator (:rule #\-))
      (,(make-event "/a" "b") ,(make-event "/c" "d"))
      "--------------------------------------------------------------------------------\\*--------------------------------------------------------------------------------\\*")

    `((:separator "&&&")
      (,(make-event "/a" "b") ,(make-event "/c" "d"))
      "&&&\\*&&&\\*")))

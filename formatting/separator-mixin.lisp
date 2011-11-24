;;; separator-mixin.lisp --- Mixin for printing separator rulers.
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

(in-package :rsb.formatting)

(defclass separator-mixin ()
  ((separator-character :initarg  :separator-character
			:type     character
			:accessor style-separator-character
			:initform #\-
			:documentation
			"The character by means of repetition of which
the separator is built.")
   (separator?          :initarg  :separator?
			:accessor style-separator?
			:type     boolean
			:initform t
			:documentation
			"Controls whether the style instance actually
prints the separator."))
  (:documentation
   "This class is intended to be mixed into style classes that should
print separators between output items."))

(defmethod format-event :after ((event  event)
				(style  separator-mixin)
				(stream t)
				&key
				(max-columns (or *print-right-margin* 80))
				&allow-other-keys)
  "Print a vertical rule after each event."
  (bind (((:accessors-r/o (separator-character style-separator-character)
			  (separator?          style-separator?)) style))
    (when separator?
      (format stream "~A~%"
	      (make-string max-columns
			   :initial-element separator-character)))))

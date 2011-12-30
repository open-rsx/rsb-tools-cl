;;; event-style-columns.lisp --- Generic column-based formatting class.
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

(cl:in-package :rsb.formatting)

(defmethod find-style-class ((spec (eql :columns)))
  (find-class 'style-columns))

(defclass style-columns (header-printing-mixin
			 columns-mixin)
  ()
  (:documentation
   "This formatting style prints configurable properties of received
events in a column-oriented fashion. Event properties and the
associated columns in which the properties should be printed have to
be specified using the :columns initarg. If no columns are specified,
no output is produced."))

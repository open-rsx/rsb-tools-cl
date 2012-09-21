;;; mock-column.lisp --- A mock column class for tests.
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

(defmethod find-column-class ((spec (eql :mock)))
  (find-class 'mock-column))

(defclass mock-column (width-mixin)
  ())

(defmethod format-header ((thing  mock-column)
			  (target t))
  (format target "My mock header"))

(defmethod format-event ((event  t)
			 (style  mock-column)
			 (target t)
			 &key &allow-other-keys)
  (format target "~A" event))

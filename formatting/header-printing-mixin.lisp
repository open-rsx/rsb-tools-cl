;;; header-printing-mixin.lisp --- Mixin for formatting styles which print headers.
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

(defclass header-printing-mixin (counting-mixin)
  ((header-frequency :initarg  :header-frequency
		     :type     (or null positive-integer)
		     :accessor style-header-frequency
		     :initform 22
		     :documentation
		     "Stores the number of output cycles after which a
header should be printed or nil in case a header is never printed."))
  (:documentation
   "This class is intended to be mixed into formatting style classes
that periodically print a header of some kind into their regular
stream of output."))

(defmethod format-event :before ((event  t)
				 (style  header-printing-mixin)
				 (stream t)
				 &key &allow-other-keys)
  (bind (((:accessors-r/o (header-frequency style-header-frequency)) style))
    (when (and header-frequency
	       (zerop (mod (style-count style) header-frequency)))
      (format-header style stream))))

;;; output-buffering-mixin.lisp --- Mixin for buffering of formatter output.
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

(defclass output-buffering-mixin ()
  ()
  (:documentation
   "This mixin class provides buffering of output. This can be useful
when lots of output has to be produced and written. This can, for
example, reduce flickering."))

(defmethod format-event :around ((event  (eql :trigger))
				 (style  output-buffering-mixin)
				 (stream t)
				 &rest args &key &allow-other-keys)
  (write-string
   (with-output-to-string (stream)
     (apply #'call-next-method event style stream args))
   stream))

;;; rate-mixin.lisp --- rate quantity mixin class.
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

(cl:in-package :rsb.stats)

(defclass rate-mixin ()
  ((start-time :initarg  :start-time
	       :accessor %quantity-start-time
	       :initform nil
	       :documentation
	       "Stores the start time of the current computation
period."))
  (:documentation
   "This class is intended to be mixed into quantity classes that
compute the rate of a quantity over a period of time. It takes care of
tracking the start and end times of time periods and turns a computed
absolute value into a rate value using this information."))

(defmethod quantity-value :around ((quantity rate-mixin))
  (let+ ((value (call-next-method))
	 ((&accessors-r/o (start-time %quantity-start-time)) quantity)
	 (now (local-time:now)))
    (if (and (realp value) start-time)
	(let ((diff (local-time:timestamp-difference now start-time)))
	  (if (plusp diff)
	      (/ value diff)
	      :n/a))
	:n/a)))

(defmethod reset! ((quantity rate-mixin))
  (setf (%quantity-start-time quantity) (local-time:now))
  (when (next-method-p)
    (call-next-method)))

;;; periodic-printing-mixin.lisp --- Mixin for timer-driven formatting.
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

(cl:in-package :rsb.formatting)

(defclass periodic-printing-mixin ()
  ((print-interval :initarg  :print-interval
		   :type     print-interval
		   :accessor style-print-interval
		   :initform 1
		   :documentation
		   "Stores the amount of time in seconds between
successive print operations.")
   (stream         :type     (or null stream)
		   :accessor %style-stream
		   :initform nil
		   :documentation
		   "Stores the stream that should be used for periodic
printing.")
   (pretty-state   :initarg  :pretty-state
		   :type     list
		   :accessor %style-pretty-state
		   :documentation
		   "Stores the pretty-printer state that should be
used for periodic printing.")
   (timer          :accessor %style-timer
		   :documentation
		   "Stores the timer used to trigger periodic
printing.")
   (lock           :reader   %style-lock
		   :initform (bt:make-recursive-lock
			      "Periodic printing lock")
		   :documentation
		   "Stores a lock that protects timer-triggered
accesses to the style object against `format-event'-triggered
accesses."))
  (:documentation
   "This mixin class is intended to be mixed into formatting classes
that produce output periodically instead of being triggered by the
arrival of events.

When `format-event' is called, an :around method prevents
output-producing methods from running. Instead, these method are run a
timer-driven way."))

(defmethod initialize-instance :after ((instance periodic-printing-mixin)
                                       &key)
  (let+ (((&accessors (timer          %style-timer)
		      (print-interval style-print-interval)) instance)
	 (timer* #+sbcl (sb-ext:make-timer (%make-timer-function instance)
					   :thread t)
		 #-sbcl #.(error "not implemented")))
    ;; Store and activate the timer.
    (setf timer          timer*
	  print-interval print-interval) ;; trigger scheduling

    ;; Register a finalizer to stop the timer. We need the timer*
    ;; variable since the finalizer closure must not perform slot
    ;; accesses on INSTANCE.
    (tg:finalize instance #'(lambda () (sb-ext:unschedule-timer timer*)))))

(defmethod (setf style-print-interval) :before ((new-value t)
						(style     periodic-printing-mixin))
  "Validate NEW-VALUE to prevent bad timer scheduling."
  (check-type new-value print-interval))

(defmethod (setf style-print-interval) :after ((new-value t)
					       (style     periodic-printing-mixin))
  "After storing the new print-interval value, reschedule the timer."
  (let+ (((&accessors-r/o (timer %style-timer)) style))
    #-sbcl #.(error "not implemented")
    #+sbcl (sb-ext:unschedule-timer timer)
    (when new-value
      #+sbcl (sb-ext:schedule-timer timer new-value
				    :repeat-interval new-value))))

(defmethod format-event :around ((event  t)
				 (style  periodic-printing-mixin)
				 (stream t)
				 &key &allow-other-keys)
  "Protect against concurrent access to STYLE and store STREAM for use
in timer-driven output."
  (bt:with-recursive-lock-held ((%style-lock style))
    (unless (eq event :trigger)
      (setf (%style-stream style)       stream
	    (%style-pretty-state style) (list *print-right-margin*
					      *print-miser-width*)))

    (call-next-method)))


;;; Utility functions
;;

(defun %make-timer-function (style)
  "Return a function that is weakly-closed over STYLE and tries to run
STYLE's `format-event' function when called."
  (let ((weak-style (tg:make-weak-pointer style)))
    #'(lambda ()
	(when-let ((style  (tg:weak-pointer-value weak-style))
		   (stream (%style-stream style)))
	  (let+ (((*print-right-margin* *print-miser-width*)
		  (%style-pretty-state style)))
	    (ignore-some-conditions (stream-error)
	      (format-event :trigger style stream)))))))

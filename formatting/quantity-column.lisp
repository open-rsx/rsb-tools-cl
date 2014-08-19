;;;; quantity-column.lisp ---
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.formatting)

(defmethod find-column-class ((spec (eql :quantity)))
  (find-class 'quantity-column))

(defclass quantity-column (width-mixin)
  ((quantity :accessor column-quantity
             :documentation
             "Stores the underlying quantity instances printed by this
              column."))
  (:default-initargs
   :quantity (missing-required-initarg 'quantity-column :quantity))
  (:documentation
   "Instances of this class use an associated quantity instance to
    provide a name and the printed value."))

(defmethod shared-initialize :after ((instance   quantity-column)
                                     (slot-names t)
                                     &key
                                     quantity)
  (when quantity
    (setf (column-quantity instance) (rsb.stats:make-quantity quantity))))

(defmethod column< ((left quantity-column) (right quantity-column))
  (value< (rsb.stats:quantity-value (column-quantity left))
          (rsb.stats:quantity-value (column-quantity right))))

(defmethod column-name ((column quantity-column))
  (rsb.stats:quantity-name (column-quantity column)))

(defmethod collects? ((column quantity-column))
  t)

(defmethod format-header ((column quantity-column)
                          (stream t))
  (format stream "~@(~A~)~@[ [~A]~]" (column-name column) nil))

(defmethod format-event :around ((event  t)
                                 (style  quantity-column)
                                 (stream t)
                                 &key &allow-other-keys)
  (if (eq event :trigger)
      (call-next-method)
      (rsb.stats:update! (column-quantity style) event)))

(defmethod format-event ((event  t)
                         (style  quantity-column)
                         (stream t)
                         &key &allow-other-keys)
  (error "Should not get called"))

(defmethod format-event ((event  (eql :trigger))
                         (style  quantity-column)
                         (stream t)
                         &key &allow-other-keys)
  (let+ (((&accessors-r/o (quantity column-quantity)) style))
    (rsb.stats:format-value quantity stream)
    (rsb.stats:reset! quantity)))

(defmethod print-object ((object quantity-column) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A = " (column-name object))
    (rsb.stats:format-value (column-quantity object) stream)))

;;;; binary-style-mixins.lisp --- Mixin classes for binary event formatting classes.
;;;;
;;;; Copyright (C) 2012-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.formatting)

;;; `data-consistency-mixin'

(defclass data-consistency-mixin ()
  ((target-descriptors :type     hash-table
                       :accessor style-%descriptors
                       :initform (make-hash-table :test #'eq)
                       :documentation
                       "Stores a mapping of output target to format
                        descriptors. All events which are emitted to
                        one target have to produce compatible
                        descriptors."))
  (:documentation
   "This mixin class provides a mechanism for ensuring consistency
    between all events which are emitted to a particular target object
    like a stream. To achieve this, descriptors are extracted from
    events and compared to detect incompatible events."))

(defmethod descriptor-for-target ((style  data-consistency-mixin)
                                  (target t))
  (gethash target (style-%descriptors style)))

(defmethod (setf descriptor-for-target) ((new-value t)
                                         (style     data-consistency-mixin)
                                         (target    t))
  (setf (gethash target (style-%descriptors style)) new-value))

(defmethod compatible-descriptors? ((style        data-consistency-mixin)
                                    (descriptor-1 t)
                                    (descriptor-2 t))
  (equal descriptor-1 descriptor-2))

(defmethod incompatible-descriptors ((style        data-consistency-mixin)
                                     (descriptor-1 t)
                                     (descriptor-2 t))
  (error "~@<Data format of current event (~A) is different from the ~
          format of previous events (~A).~@:>"
         descriptor-1 descriptor-2))

(defmethod format-payload :before ((data   t)
                                   (style  data-consistency-mixin)
                                   (target t)
                                   &key)
  (let* ((data-descriptor   (make-descriptor style data target))
         (target-descriptor (descriptor-for-target style target)))
    (cond
      ((null target-descriptor)
       (setf (descriptor-for-target style target) data-descriptor))

      ((not (compatible-descriptors? style target-descriptor data-descriptor))
       (incompatible-descriptors style data-descriptor target-descriptor)))))

(defmethod print-object ((object data-consistency-mixin) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "(~D)" (hash-table-count
                           (style-%descriptors object)))))

;;; `image-output-mixin'

(defclass image-output-mixin ()
  ((width  :type     dimension-spec/full
           :accessor style-width
           :initform t
           :documentation
           "Stores the desired width of output images produced by the
            style instance.")
   (height :type     dimension-spec/full
           :accessor style-height
           :initform t
           :documentation
           "Stores the desired height of output images produced by the
            style instance."))
  (:documentation
   "This formatting style outputs image data in common image formats
    for event payloads consisting of image data."))

(defmethod shared-initialize :after ((instance   image-output-mixin)
                                     (slot-names t)
                                     &key
                                     width
                                     height)
  (when width
    (check-type width dimension-spec)
    (setf (style-width instance) (normalize-dimension-spec width)))
  (when height
    (check-type height dimension-spec)
    (setf (style-height instance) (normalize-dimension-spec height))))

(defmethod print-object ((object image-output-mixin) stream)
  (let+ (((&flet spec-for-format (spec)
            (if (eq spec t) '(:keep) (reverse spec)))))
    (print-unreadable-object (object stream :type t :identity t)
      (format stream "~{~(~A~)~^ ~} x ~{~(~A~)~^ ~}"
              (spec-for-format (style-width object))
              (spec-for-format (style-height object))))))

;; Utility functions

(declaim (ftype (function (dimension-spec) dimension-spec/full)
                normalize-dimension-spec))

(defun normalize-dimension-spec (spec)
  "Expand the possibly abbreviated dimension specification SPEC into a
   full dimension specification."
  (etypecase spec
    (positive-integer
     (list :px spec))
    (positive-real
     (list :%  spec))
    (dimension-spec/full
     spec)))

(declaim (ftype (function (positive-integer dimension-spec/full)
                          positive-integer)
                apply-dimension-spec))

(defun apply-dimension-spec (input spec)
  "Compute output dimension by applying SPEC to INPUT."
  (etypecase spec
    ((eql t)
     input)
    ((cons (eql :px) (cons positive-integer null))
     (second spec))
    ((cons (eql :%)  (cons positive-real null))
     (values (floor (* input 1/100 (second spec)))))))

(declaim (ftype (function (positive-integer dimension-spec/full)
                          (values positive-integer positive-integer))
                apply-dimension-spec/integer-factor))

(defun apply-dimension-spec/integer-factor (input spec)
  "Compute output dimension by applying SPEC to INPUT and ensuring
   that the result is an integer fraction of INPUT. Return the output
   dimension and the integer scaling factor."
  (let* ((requested (apply-dimension-spec input spec))
         (scale     (round (/ input requested))))
    (values (floor input scale) scale)))

(macrolet
    ((define-yuv->rgb (name alpha?)
       `(progn
          (declaim (inline ,name)
                   (ftype (function ((nibbles:simple-octet-vector) fixnum
                                     (nibbles:simple-octet-vector) fixnum)
                                    (values))
                          ,name))

          (defun ,name (yuv-array yuv-start rgb-array rgb-start)
            (declare #.rsb:+optimization-fast+unsafe+)

            (let* ((c     (aref yuv-array (+ yuv-start 0)))
                   (d     (aref yuv-array (+ yuv-start 1)))
                   (c2    (aref yuv-array (+ yuv-start 2)))
                   (e     (aref yuv-array (+ yuv-start 3)))

                   (valc1 (+ (* 298 c)  128 (* 298 -16)))
                   (valc2 (+ (* 298 c2) 128 (* 298 -16)))

                   (blue  (+ (* 517 d)              (* 517 -128)))
                   (green (- (+ (* 208 d) (* 100 e) (* 208 -128) (* 100 -128))))
                   (red   (+ (* 409 e)              (* 409 -128))))
              (macrolet
                  ((store (offset form)
                     `(setf (aref rgb-array (+ rgb-start ,offset))
                            (clamp (ash ,form -8) 0 255))))
                (store 0 (+ valc1 red))
                (store 1 (+ valc1 green))
                (store 2 (+ valc1 blue))
                ,@(if alpha?
                      `((store 3 65535)
                        (store 4 (+ valc2 red))
                        (store 5 (+ valc2 green))
                        (store 6 (+ valc2 blue))
                        (store 7 65535))
                      `((store 3 (+ valc2 red))
                        (store 4 (+ valc2 green))
                        (store 5 (+ valc2 blue))))))
            (values)))))

  (define-yuv->rgb %yuv422->rgba t)
  (define-yuv->rgb %yuv422->rgb  nil))

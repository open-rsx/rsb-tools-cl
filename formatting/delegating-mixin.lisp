;;;; delegating-mixin.lisp --- Predicate-based delegation to sub-styles.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsb.formatting)

(defclass delegating-mixin ()
  ((sub-styles :initarg  :sub-styles
               :type     list
               :accessor style-sub-styles
               :initform nil
               :documentation
               "Stores predicates and corresponding sub-styles as an
alist of items of the form (PREDICATE . SUB-STYLE)."))
  (:documentation
   "This class is intended to be used in formatting classes that
delegate to sub-styles based on dispatch predicates."))

(defmethod sub-style-for ((style delegating-mixin)
                          (event t))
  "Return a list of sub-styles of STYLE whose predicates succeed on
EVENT."
  (map 'list #'cdr
       (remove-if (complement (rcurry #'funcall event))
                  (style-sub-styles style)
                  :key #'car)))

(defmethod delegate ((event  t)
                     (style  delegating-mixin)
                     (stream t))
  (let+ ((sub-styles (sub-style-for style event))
         ((&labels apply-style (style-or-styles)
            (cond
              ((typep style-or-styles 'sequence)
               (map nil #'apply-style style-or-styles))
              (t
               (format-event event style-or-styles stream))))))
    (map nil #'apply-style sub-styles)))

(defmethod format-event ((event  t)
                         (style  delegating-mixin)
                         (stream t)
                         &key &allow-other-keys)
  "Delegate formatting of EVENT on STREAM to appropriate sub-styles of
STYLE."
  (delegate event style stream))

(defmethod print-object ((object delegating-mixin) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "(~D)" (length (style-sub-styles object)))))

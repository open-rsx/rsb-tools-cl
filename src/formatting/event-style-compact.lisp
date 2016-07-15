;;;; event-style-compact.lisp --- Compact event formatting style class.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.formatting)

;;; Class `event-style-compact-line'

(defclass event-style-compact-line (columns-mixin
                                    widths-caching-mixin)
  ())

;;; Class `event-style-compact'

(defun default-compact-sub-styles ()
  (let+ (((&flet+ make-sub-style ((predicate . spec))
            (cons predicate (make-instance 'event-style-compact-line
                                           :columns spec))))
         (dummy '(:constant :name "" :value "" :width 0)))
    (mapcar (lambda+ ((predicate . column-specs))
              (make-sub-style (cons predicate
                                    (mapcar #'expand-column-spec column-specs))))
            `((,#'request-event? . (:receive
                                    :id :call ,dummy ,dummy
                                    :wire-schema :data-size :origin :sequence-number
                                    :newline))
              (,#'reply-event?   . (:receive
                                    :call-id :result ,dummy ,dummy
                                    :wire-schema :data-size :origin :sequence-number
                                    :newline))
              (,(constantly t)   . (:receive
                                    :id :method :scope :data
                                    :wire-schema :data-size :origin :sequence-number
                                    :newline))))))

(defclass event-style-compact (delegating-mixin
                               header-printing-mixin)
  ()
  (:default-initargs
   :sub-styles (default-compact-sub-styles))
  (:documentation
   "This formatting style prints several properties of received events
    on a single line. Some events are formatted specially according to
    their role in a communication pattern."))

(service-provider:register-provider/class
 'style :compact :class 'event-style-compact)

(defmethod sub-style-for ((style event-style-compact)
                          (event t))
  ;; Return a singleton list containing the sub-style of STYLE whose
  ;; predicate succeeds on EVENT.
  (ensure-list
   (cdr (find-if (rcurry #'funcall event) (style-sub-styles style)
                 :key #'car))))

(defmethod format-header ((style  event-style-compact)
                          (stream t))
  ;; Use header of the last sub-style which is the most generic one.
  (format-header (cdr (lastcar (style-sub-styles style))) stream))

(defmethod format-event :before ((event  t)
                                 (style  event-style-compact)
                                 (stream t)
                                 &key
                                 (width (or *print-right-margin* 80)))
  (let+ (((&structure-r/o style- sub-styles) style)
         ((&flet do-sub-style (sub-style)
            (let+ ((columns   (style-dynamic-width-columns sub-style))
                   (separator (style-separator-width       sub-style))
                   ((&values widths cached?) (style-compute-column-widths
                                              sub-style columns width
                                              :separator-width separator)))
              (unless cached?
                (style-assign-column-widths sub-style columns widths))
              (values columns widths cached?))))
         ;; Compute widths for final sub-style which is the most
         ;; generic one.
         ((&values columns widths cached?)
          (do-sub-style (cdr (lastcar sub-styles))))
         ((&flet do-subordinate-sub-style (sub-style)
            (map nil (lambda (column1 column2 width)
                       (when (type= (type-of column1) (type-of column2))
                         (setf (column-widths column1) width)))
                 (style-dynamic-width-columns sub-style)
                 columns widths)
            (do-sub-style sub-style))))
    ;; Assign column widths computed for final sub-style to shared
    ;; columns of other sub-styles. Then optimize remaining ("free")
    ;; column widths in these sub-styles.
    (unless cached?
      (mapc (compose #'do-subordinate-sub-style #'cdr)
            (butlast sub-styles)))))

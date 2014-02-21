;;;; event-style-statistics.lisp --- Column-oriented formatting of statistics.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.formatting)

;;; Class `statistics-columns-mixin'

(defclass statistics-columns-mixin (columns-mixin)
  ()
  (:documentation
   "This class is intended to be mixed into formatting classes that
    present the values of statistical quantities in a column-based
    manner."))

(defmethod collects? ((style statistics-columns-mixin))
  t)

(defmethod format-event :around ((event  t)
                                 (style  statistics-columns-mixin)
                                 (stream t)
                                 &key &allow-other-keys)
  ;; Update quantities.
  (if (eq event :trigger)
      (call-next-method)
      (map nil (lambda (column)
                 (when (collects? column)
                   (format-event event column stream)))
           (style-columns style))))

;;; Classes `style-statistics/*'
;;;
;;; These provide increasingly detailed statistics formatting.

(macrolet
    ((define-statistics-style ((name
                                &key
                                (spec       (make-keyword name))
                                (class-name (symbolicate :style "-" name)))
                               &body doc-and-column-specs)
       (let+ (((&values column-specs nil documentation)
               (parse-body doc-and-column-specs :documentation t)))
         `(progn
            (defmethod find-style-class ((spec (eql ,spec)))
              (find-class ',class-name))

            (defclass ,class-name (periodic-printing-mixin
                                   statistics-columns-mixin
                                   header-printing-mixin)
              ()
              (:default-initargs
               :columns (list ,@(sublis (mapcar (lambda+ ((key . value))
                                                  `(,key . (quote ,value)))
                                                *basic-columns*)
                                        column-specs)))
              (:documentation
               ,(format nil "This formatting style computes a number ~
                             of configurable statistical quantities ~
                             from received events collected over a ~
                             configurable period of time and prints ~
                             the computed values in a tabular manner.~
                             ~@[ ~A~]"
                        documentation)))))))

  (define-statistics-style (statistics/80)
      "The output of this style is designed to fit into 80 columns."
    :now/compact :rate/12 :throughput/13 :latency :size/20
    :newline)

  (define-statistics-style (statistics/128)
      "The output of this style is designed to fit into 128 columns."
    :now/compact :rate/12 :throughput/13 :latency :scope/40 :size/20
    :newline)

  (define-statistics-style (statistics/180)
      "The output of this style is designed to fit into 180 columns."
    :now :rate/12 :throughput/13 :latency :origin/40 :scope/40 :size/20
    :newline)

  (define-statistics-style (statistics/220)
      "The output of this style is designed to fit into 180 columns."
    :now :rate/12 :throughput/13 :latency :origin/40 :scope/40 :type/40 :size/20
    :newline))

;;; Class `style-statistics'
;;;
;;; Statistics meta-style that dispatches to one of the statistics
;;; styles based on available horizontal room.

(define-dynamic-width-style (statistics
                             :superclasses (periodic-printing-mixin))
  ((  0   81) (make-instance 'style-statistics/80 :print-interval nil))
  (( 81  129) (make-instance 'style-statistics/128 :print-interval nil))
  ((129  181) (make-instance 'style-statistics/180 :print-interval nil))
  ((181     ) (make-instance 'style-statistics/220 :print-interval nil)))

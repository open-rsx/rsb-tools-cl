;;;; event-style-compact.lisp --- Compact event formatting style class.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.formatting)

;;; Class `basic-compact' style

(defclass basic-compact-style (delegating-mixin
                               header-printing-mixin)
  ()
  (:documentation
   "This class is intended to be used as a superclass for compact
style classes."))

(defmethod sub-style-for ((style basic-compact-style)
                          (event t))
  "Return a singleton list containing the sub-style of STYLE whose
predicate succeeds on EVENT."
  (ensure-list
   (cdr (find-if (rcurry #'funcall event) (style-sub-styles style)
                 :key #'car))))

(defmethod format-header ((style  basic-compact-style)
                          (stream t))
  (format-header
   (cdr (lastcar (style-sub-styles style))) stream))

;;; Classes `style-compact/*'
;;;
;;; These provide increasingly detailed "compact" event formatting.

(macrolet
    ((define-compact-style ((name
                             &key
                             (spec       (make-keyword name))
                             (class-name (symbolicate :style "-" name)))
                            &body doc-and-sub-styles)
       (let+ (((&values sub-styles nil documentation)
               (parse-body doc-and-sub-styles :documentation t))
              ((&flet+ make-sub-style ((predicate . spec))
                 `(cons
                   ,predicate
                   (make-instance 'columns-mixin
                                  :columns ',spec)))))
        `(progn
           (defmethod find-style-class ((spec (eql ,spec)))
             (find-class ',class-name))

           (defclass ,class-name (basic-compact-style)
             ()
             (:default-initargs
              :sub-styles (list ,@(map 'list #'make-sub-style
                                       sub-styles)))
             (:documentation
              ,(apply #'concatenate 'string
                      "This formatting style prints several properties
of received events on a single line. Some events are formatted
specially according to their role in a communication pattern."
                      (when documentation
                        (list " " documentation)))))))))

  (define-compact-style (compact/80)
      "The output of this style is designed to fit into 80 columns."
    (#'request-event? . (:now/compact
                         :origin
                         (:call :width 52)
                         :newline))
    (#'reply-event?   . (:now/compact
                         :origin
                         (:result :width 52)
                         :newline))
    ((constantly t)   . (:now/compact
                         :origin
                         (:scope :width 22)
                         (:data :width 22) :data-size
                         :newline)))

  (define-compact-style (compact/128)
      "The output of this style is designed to fit into 128 columns."
    (#'request-event? . (:now/compact
                         :origin :sequence-number :id :call :data-size
                         :newline))
    (#'reply-event?   . (:now/compact
                         :origin :sequence-number :call-id :result :data-size
                         :newline))
    ((constantly t)   . (:now/compact
                         :origin :sequence-number :id :method
                         (:scope :width 29)
                         (:data  :width 34) :data-size
                         :newline)))

  (define-compact-style (compact/180)
      "The output of this style is designed to fit into 180 columns."
    (#'request-event? . (:now
                         :origin :sequence-number :id
                         (:call :width 85) :wire-schema :data-size
                         :newline))
    (#'reply-event?   . (:now
                         :origin :sequence-number :call-id
                         (:result :width 85) :wire-schema :data-size
                         :newline))
    ((constantly t)   . (:now
                         :origin :sequence-number :id :method
                         (:scope :width 32)
                         (:data :width 41) :wire-schema :data-size
                         :newline))))

;;; Class `style-compact'
;;;
;;; Compact meta-style that dispatches to one of the compact styles
;;; based on available horizontal room.

(define-dynamic-width-style (compact)
  ((  0   81) (make-instance 'style-compact/80))
  (( 81  129) (make-instance 'style-compact/128))
  ((129     ) (make-instance 'style-compact/180)))

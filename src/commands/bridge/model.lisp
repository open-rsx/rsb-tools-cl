;;;; model.lisp --- Description of bridge configurations.
;;;;
;;;; Copyright (C) 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.tools.commands.bridge)

;;; Model classes

(macrolet
    ((define-model-class
         ((kind
           &key
           (name        (format-symbol *package* "~A-~A" kind '#:description))
           (constructor (format-symbol *package* "~A-~A" '#:make name)))
          &rest slots)
       (let+ (((&flet+ composite-slot? ((&ign kind &rest &ign))
                 (not (eq kind :scalar))))
              (scalar-slots    (remove-if #'composite-slot? slots))
              (composite-slots (set-difference slots scalar-slots))
              ((&flet+ make-scalar-slot ((name kind &key type))
                 `(,name nil :type ,type :read-only t)))
              ((&flet+ make-composite-slot ((name kind &key type))
                 (etypecase kind
                   ((cons (eql :composite) (cons (eql  1) null))
                    `(,name nil :type ,type))
                   ((cons (eql :composite) (cons (eql ?) null))
                    `(,name nil :type (or null ,type)))
                   ((cons (eql :composite) (cons (eql *) null))
                    `(,name nil :type list)))))
              ((&flet+ make-keyword-parameter((name &rest &ign))
                 `(,name (missing-required-argument ,(make-keyword name)))))
              ((&flet+ make-initarg ((name &rest &ign))
                 `(,(make-keyword name) ,name)))
              ((&flet+ make-relate-method (name (slot-name slot-kind &key type))
                 (let ((accessor-name (symbolicate name '#:- slot-name)))
                   `(defmethod architecture.builder-protocol:relate
                        ((builder  t)
                         (relation (eql ,(make-keyword slot-name)))
                         (left     ,name)
                         (right    ,type)
                         &key)
                      ,(etypecase slot-kind
                         ((cons (eql :composite) (cons (member 1 ?()) null))
                          `(setf (,accessor-name left) right))
                         ((cons (eql :composite) (cons (eql *) null))
                          `(appendf (,accessor-name left) (list right))))
                      left)))))
         `(progn
            (defstruct (,name (:predicate nil) (:copier nil))
              ,@(mapcar #'make-scalar-slot scalar-slots)
              ,@(mapcar #'make-composite-slot composite-slots))

            (defmethod architecture.builder-protocol:make-node
                ((builder t) (kind (eql ,kind))
                 &key
                 ,@(mapcar #'make-keyword-parameter scalar-slots))
              (,constructor ,@(mapcan #'make-initarg scalar-slots)))

            ,@(mapcar (curry #'make-relate-method name)
                      composite-slots)))))

  (define-model-class (:input)
    (uri :scalar :type puri:uri))

  (define-model-class (:output)
    (uri :scalar :type puri:uri))

  (define-model-class (:filter)
    (class    :scalar :type symbol)
    (initargs :scalar :type list))

  (define-model-class (:transform)
    (class    :scalar :type symbol)
    (initargs :scalar :type list))

  (define-model-class (:connection)
    (inputs          (:composite *) :type input-description)
    (outputs         (:composite *) :type output-description)
    (filters         (:composite *) :type filter-description)
    (transform       (:composite ?) :type transform-description)
    (other-direction (:composite 1) :type t))

  (define-model-class (:bridge)
    (connections (:composite *) :type connection-description)))

;;; Model checks

(defmethod communication? ((output output-description)
                           (input  input-description))
  ;; TODO consider filters and configuration
  (communication? (output-description-uri output)
                  (input-description-uri input)))

;; Check DESCRIPTION for problems and either signal a (continuable)
;; error and/or return two values: 1) the checked DESCRIPTION 2) a
;; list of "problems" of the form
;;
;;   (INPUT . OUTPUTS)
;;
;; where INPUT is an `input-description' and OUTPUTS is a list of
;; `output-description' such that INPUT would receive events sent by
;; each element of OUTPUTS.
(defun check-description (description)
  (let+ (((&structure-r/o bridge-description- connections) description)
         (problems ())
         ((&flet maybe-problem (kind input  input-connection
                                     output output-connection)
            (when (and (eq output-connection
                           (connection-description-other-direction
                            input-connection))
                       (eq input-connection
                           (connection-description-other-direction
                            output-connection)))
              (return-from maybe-problem))
            (funcall
             (ecase kind
               (error
                (lambda (&rest args)
                  (with-simple-restart (continue "Continue anyway")
                    (apply #'error 'forwarding-cycle-error args))))
               (warning
                (curry #'warn 'forwarding-cycle-warning)))
             :source      input
             :destination output)))
         ((&flet record (input output)
            (if-let ((cell (assoc input problems)))
              (push output (cdr cell))
              (push (cons input (list output)) problems))))
         ((&flet+ check-pair ((output output-connection)
                              (input  input-connection))
            (let+ (((&values result definitive?)
                    (communication? output input)))
              (cond
                ((and result definitive?)
                 (maybe-problem 'error input  input-connection
                                       output output-connection)
                 (record input output))
                ((and (not result) (not definitive?))
                 (maybe-problem 'warning input  input-connection
                                         output output-connection)
                 (record input output))))))
         (all-inputs  '())
         (all-outputs '())
         ((&flet add-inputs+outputs (connection)
            (let+ (((&structure-r/o connection-description- inputs outputs)
                    connection))
              (appendf all-inputs
                       (mapcar #'list inputs  (circular-list connection)))
              (appendf all-outputs
                       (mapcar #'list outputs (circular-list connection)))))))
    (mapc #'add-inputs+outputs connections)
    (map-product #'check-pair all-outputs all-inputs)
    (values description problems)))

;;;; grammar.lisp --- Grammar for simple forwarding specification .
;;;;
;;;; Copyright (C) 2015, 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.tools.commands.bridge)

;;; Rules

(defrule skippable
    whitespace+)

(defrule skippable?
    whitespace*)

(defrule bridge-specification
    connection-list
  (:lambda (connections)
    (bp:node* (:bridge)
      (* :connections connections))))

(defrule connection-list
    (or (and (+ connection+semicolon/?s) connection)
        (and (and connection)            (and)))
  (:destructure (rest last)
    (reduce #'append rest :initial-value last)))

(defrule/s connection+semicolon
    (and connection/?s semicolon-keyword)
  (:function first))

(defrule/s connection
    (or connection/bidirectional connection/unidirectional))

(let+ (((&flet make-connection (inputs outputs
                                &optional filters transform)
          (bp:node* (:connection)
            (* :inputs    inputs)
            (* :outputs   outputs)
            (* :filters   filters)
            (1 :transform transform)))))

  (defrule connection/unidirectional
      (and (+ input/?s)
           right-arrow-keyword/?s
           (* filter/?s) (? transform/?s)
           output-list)
    (:destructure (inputs operator filters transform outputs)
      (declare (ignore operator))
      (list (make-connection inputs outputs filters transform))))

  (defrule connection/bidirectional
      (and (+ input/?s) left-right-arrow-keyword/?s output-list)
    (:destructure (inputs operator outputs)
      (declare (ignore operator))
      (let+ (((&flet extract-uri (description)
                (etypecase description
                  (input-description  (input-description-uri description))
                  (output-description (output-description-uri description)))))
             ((&flet input->output (input)
                (bp:node* (:output :uri (extract-uri input)))))
             ((&flet output->input (output)
                (bp:node* (:input :uri (extract-uri output)))))
             (to   (make-connection inputs outputs))
             (from (make-connection (mapcar #'output->input outputs)
                                    (mapcar #'input->output inputs))))
        (bp:relate* :other-direction to from)
        (bp:relate* :other-direction from to)
        (list to from)))))

(defrule output-list
    (or (and (+ output-with-successor) (and output))
        (and (and output)              (and)))
  (:destructure (rest last)
    (append rest last)))

(defrule output-with-successor
    (and output/?s (& output))
  (:function first))

(defrule/s input
    uri
  (:lambda (uri)
    (bp:node* (:input :uri uri))))

(defrule/s output
    uri
  (:lambda (uri)
    (bp:node* (:output :uri uri))))

(macrolet
    ((define-rule (name open-keyword close-keyword kind)
       `(defrule/s ,name
          (and ,open-keyword (+ (not ,close-keyword)) ,close-keyword)
        (:function second)
        (:text t)
        (:function parse-instantiation-spec)
        (:destructure (class &rest initargs)
          (bp:node* (,kind :class class :initargs initargs))))))

  (define-rule filter    pipe-keyword/?s  pipe/end  :filter)
  (define-rule transform slash-keyword/?s slash/end :transform))

(defrule pipe/end
    (and (& (and pipe-keyword/?s (* filter/?s) (? transform/?s) output-list))
         pipe-keyword))

(defrule slash/end
    (and (& (and slash-keyword/?s output-list)) slash-keyword))

(defun uri-absolute-path? (uri)
  (let ((path (puri:uri-parsed-path uri)))
    (or (emptyp path) (starts-with :absolute path))))

(defrule/s uri
    (uri-absolute-path? parsable-uri))

(defun parsable-uri? (uri)
  (ignore-errors (puri:parse-uri uri)))

(defrule parsable-uri
    (parsable-uri? uri-characters)
  (:function puri:parse-uri))

(defrule uri-characters
    (+ (or (not (or skippable ";" "->" "<->")) uri-characters/special))
  (:text t))

(defrule uri-characters/special
    (and (or ";" "->" "<->") (& (or uri-characters
                                    ";" "->" "<->"
                                    (not character))))
  (:function first))

(macrolet ((define-keyword-rule (name string)
             (let ((rule-name (symbolicate name '#:-keyword)))
               `(defrule/s ,rule-name
                    ,string
                  (:constant ,(make-keyword name))))))
  (define-keyword-rule pipe             #\|)
  (define-keyword-rule slash            #\/)
  (define-keyword-rule right-arrow      "->")
  (define-keyword-rule left-right-arrow "<->")
  (define-keyword-rule semicolon        #\;))

(defun parse-spec (source &key (rule 'bridge-specification))
  (esrap:parse rule source))

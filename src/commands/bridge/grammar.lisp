;;;; grammar.lisp --- Grammar for simple forwarding specification .
;;;;
;;;; Copyright (C) 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.tools.commands.bridge)

(defmacro defrule/s (name-and-options expression &body options)
  "Like `esrap:defule' but define additional rules named NAME/S and
   NAME/?S which respectively require/ allow EXPRESSION to be
   followed by whitespace.

   NAME-AND-OPTIONS can either just be a rule name or list of the form

     (NAME &key WHITESPACE-RULE WS? ?WS? DEFINER)

   where WHITESPACE-RULE names the rule used to parsed whitespace in
   the NAME/S and NAME/?S variants. Defaults to `whitespace'.

   WS? and ?WS? control which of the NAME/S and NAME/?S rules should
   be generated. Default is generating both.

   DEFINER is the name of the macro used to define \"main\"
   rule. Defaults to `esrap:defrule'."
  (let+ (((name
           &key
           (skippable-rule 'whitespace)
           (ws?            t)
           (?ws?           t)
           (definer        'defrule))
          (ensure-list name-and-options))
         (name/s  (format-symbol *package* "~A/S" name))
         (name/?s (format-symbol *package* "~A/?S" name)))
    `(progn
       (,definer ,name
                 ,expression
                 ,@options)
       ,@(when ws?
           `((defrule ,name/s
                 (and ,name ,skippable-rule)
               (:function first))))
       ,@(when ?ws?
           `((defrule ,name/?s
                 (and ,name (? ,skippable-rule))
               (:function first)))))))

;;; Rules

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

(let+ (((&flet make-connection (inputs outputs &optional filters)
          (bp:node* (:connection)
            (* :inputs  inputs)
            (* :outputs outputs)
            (* :filters filters)))))

  (defrule connection/unidirectional
      (and (+ input/?s) right-arrow-keyword/?s (* filter/?s) output-list)
    (:destructure (inputs operator filters outputs)
      (declare (ignore operator))
      (list (make-connection inputs outputs filters))))

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

(defrule/s filter
    (and pipe-keyword/?s (+ (not pipe/end)) pipe/end)
  (:function second)
  (:text t)
  (:lambda (spec)
    (let+ (((class &rest initargs)
            (rsb.common:parse-instantiation-spec spec)))
      (bp:node* (:filter :class class :initargs initargs)))))

(defrule pipe/end
    (and (& (and pipe-keyword/?s (* filter/?s) output-list))
         pipe-keyword))

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
    (+ (or (not (or whitespace ";" "->" "<->")) uri-characters/special))
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
  (define-keyword-rule right-arrow      "->")
  (define-keyword-rule left-right-arrow "<->")
  (define-keyword-rule semicolon        #\;))

(defrule whitespace
    (or #\Space #\Tab))

(defun parse-spec (source)
  (esrap:parse 'bridge-specification source))

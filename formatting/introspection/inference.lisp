;;;; inference.lisp --- TODO.
;;;;
;;;; Copyright (C) 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

;;;; Participant "connections" TODO new package like rsb.meta.inference?

(cl:in-package #:rsb.formatting.introspection)

(defgeneric entities-communicate? (from to)
  (:documentation
   "TODO"))

(defgeneric participants-communicate-using-kinds? (form to from-kind to-kind)
  (:documentation
   "TODO"))

(macrolet ((define-entities-communicate?-methods (class accessor)
             `(progn
                (defmethod entities-communicate? ((from ,class)
                                                  (to   t))
                  (when-let ((processes (,accessor from)))
                    (some (rcurry #'entities-communicate? to) processes)))

                (defmethod entities-communicate? ((from t)
                                                  (to   ,class))
                  (when-let ((processes (,accessor to)))
                    (some (curry #'entities-communicate? from) processes))))))
  (define-entities-communicate?-methods host-entry
    introspection-processes)
  (define-entities-communicate?-methods process-entry
    introspection-participants/roots))

(defmethod entities-communicate? ((from participant-entry)
                                  (to   participant-entry))
  (or (entities-communicate? (entry-info from) (entry-info to))
      (when-let ((children (entry-children from)))
        (some (rcurry #'entities-communicate? to) children))
      (when-let ((children (entry-children to)))
        (some (curry #'entities-communicate? from) children))))

(defmethod entities-communicate? ((from participant-info)
                                  (to   participant-info))
  (participants-communicate-using-kinds?
   from to (participant-info-kind from) (participant-info-kind to)))

(defmethod participants-communicate-using-kinds?
    ((from      participant-info)
     (to        participant-info)
     (from-kind t)
     (to-kind   t))
  nil)

(defmethod participants-communicate-using-kinds?
    ((from      participant-info)
     (to        participant-info)
     (from-kind (eql :informer))
     (to-kind   (eql :listener)))
  (sub-scope? (participant-info-scope from) (participant-info-scope to)))

(defmethod participants-communicate-using-kinds?
    ((from      participant-info)
     (to        participant-info)
     (from-kind (eql :local-method))
     (to-kind   (eql :remote-method)))
  (scope= (participant-info-scope from) (participant-info-scope to)))

(defmethod participants-communicate-using-kinds?
    ((from      participant-info)
     (to        participant-info)
     (from-kind (eql :remote-method))
     (to-kind   (eql :local-method)))
  (scope= (participant-info-scope from) (participant-info-scope to)))

;;;; types.lisp --- Types used in the formatting module.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.formatting)

;;; Time-related types

(deftype timestamp/unix/nsec ()
  "Time in nanoseconds since UNIX epoch."
  'non-negative-integer)

;; Time specs

(deftype time-spec/variable ()
  "Time specifications which refer to variables."
  '(member :now :newest))

(deftype time-spec/operator ()
  "Operators which can occur in time specifications."
  '(member + - * / mod))

(deftype time-spec ()
  "Time specification expressions allowing constants, variables and
   operators."
  '(or time-spec/variable
       real
       (cons time-spec/operator
             (cons (satisfies time-spec-p)
                   (cons (satisfies time-spec-p) null)))))

(defun time-spec-p (thing)
  "Return non-nil, if THING is of type `time-spec'."
  (typep thing 'time-spec))

;; Bounds and bound specs

(deftype bounds-spec ()
  "A list of the form

     (LOWER-BOUND UPPER-BOUND)

   where LOWER-BOUND and UPPER-BOUND are `time-spec's."
  '(cons time-spec (cons time-spec null)))

(defun boundsp (thing)
  "Return non-nil if THING is of type `bounds'."
  (and (typep thing '(cons timestamp/unix/nsec
                           (cons timestamp/unix/nsec
                                 null)))
       (< (first thing) (second thing))))

(deftype bounds ()
  "A list of the form

     (LOWER-BOUND UPPER-BOUND)

   where LOWER-BOUND and UPPER-BOUND are of type `timestamp/unix/nsec'
   and LOWER-BOUND represents and earlier time than UPPER-BOUND."
  '(satisfies boundsp))

;;;

(deftype rule-spec ()
  "A rule specification of the form (:RULE CHARACTER)."
  '(cons (eql :rule) (cons character null)))

(deftype separator-spec ()
  "This type consists of several possible separator specifications."
  `(or null
       character string
       rule-spec
       (eql :clear)
       (cons (not keyword) list)))

;;; Width specification

(defun width-specification? (thing)
  (labels ((rec (thing)
             (typecase thing
               (non-negative-integer
                t)
               ((cons (eql :range)
                      (cons non-negative-integer
                            (or null
                                (cons positive-integer null))))
                (let+ (((&ign lower &optional upper) thing))
                  (or (not upper) (<= lower upper))))
               (cons
                (every #'rec thing)))))
    (rec thing)))

(deftype width-specification ()
  "Specification or list of specifications of the form

     POSITIVE-INTEGER | (:range MIN [MAX])

   where MIN and MAX, if both supplied are positive integers such that
   MIN <= MAX."
  '(satisfies width-specification?))

;;; Periodic printing

(deftype print-interval ()
  "Print interval specifications."
  '(or null positive-real))

;;; Image-related types.

(deftype dimension-spec/short ()
  "Short specification of an image dimension."
  '(or positive-integer
       positive-real
       (eql t)))

(deftype dimension-spec/full ()
  "Full specification of an image dimension."
  '(or (cons (eql :px) (cons positive-integer null))
       (cons (eql :%)  (cons positive-real    null))
       (eql t)))

(deftype dimension-spec ()
  "Either short or full specification of an image dimension."
  '(or dimension-spec/short
       dimension-spec/full))

;;;

(deftype template-designator ()
  "A thing that can be converted into a formatting template."
  '(or string stream pathname))

(deftype script-designator ()
  "A thing that can be converted into a formatting script."
  '(or list string stream pathname))

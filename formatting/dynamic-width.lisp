;;;; dynamic-width.lisp --- Tools for dynamic width columns.
;;;;
;;;; Copyright (C) 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

;;;; The code in this file implements a probabilistic optimization
;;;; algorithm for column widths based on simulated annealing.
;;;;
;;;; The algorithm accepts an available total width, allowed widths
;;;; for all columns in form of lists of integers or integer ranges
;;;; and priorities for all columns. A width for each column is chosen
;;;; such that an overall "score" based on respective column widths
;;;; and priorities is maximized.
;;;;
;;;; Columns can be hidden if the available width is not sufficient to
;;;; include all columns and separator strings between columns can be
;;;; taken into account.
;;;;
;;;; The optimization algorithm operates probabilistically but with a
;;;; fixed random seed resulting in potential random fluctuations
;;;; between results for different inputs but identical results for
;;;; repeated runs with a fixed input.

(cl:in-package #:rsb.formatting)

;;; Generator for width specifications

(defun make-widths (spec max)
  (labels ((rec (spec)
             (etypecase spec
               (integer
                (list spec))
               ((cons (eql :range))
                (iter (for i :from (second spec) :to (or (third spec) max))
                      (collect i)))
               (cons
                (mapcan #'rec spec)))))
    (coerce (or (rec spec) '(0)) 'vector)))

;;; Sampling utilities

(declaim (inline random-but-not))
(defun random-but-not (max exception
                       &optional (random-state *random-state*))
  (do ((value (random max random-state) (random max random-state)))
      ((/= value exception) value)))

(declaim (inline random-elt-between))
(defun random-elt-between (range min max
                           &optional (random-state *random-state*))
  (declare (type simple-vector range))
  (let* ((position-min (or (position min range :test #'<=) 0))
         (position-max (or (position max range :test #'<)  (length range)))
         (count        (- position-max position-min))
         (offset       (if (zerop count) 0 (random count random-state))))
    (elt range (+ position-min offset))))

;;; Simulated annealing algorithm

(define-constant +optimize-widths-score-table+
    (iter (for i :from 0 :to 1000)
          (collect (if (zerop i) 0 (ceiling (* 100000 (sqrt (1+ i)))))
            :result-type simple-vector))
  :test #'equalp)

(define-constant +optimize-widths-random-state+
    (make-random-state)
  :test #'equalp)

(defun %optimize-widths (width specs priorities separator-width)
  (let+ ((random-state (make-random-state +optimize-widths-random-state+))
         (priorities   (coerce priorities 'simple-vector))
         (max-priority (reduce #'max priorities))
         ((&flet unweighted-score (width)
            (declare (type fixnum width))
            (if (< width (length +optimize-widths-score-table+))
                (svref +optimize-widths-score-table+ width)
                (values (ceiling (* 100000 (sqrt (1+ width))))))))
         ;; Collect available widths and initial widths of columns,
         ;; number of columns, initial score and width of separators.
         ((&values values widths count score separator-sum)
          (iter (for i        :from      0)
                (for spec     :in        specs)
                (for priority :in-vector priorities)
                (let* ((values (make-widths spec width))
                       (min    (first-elt values)))
                  (summing min :into sum)
                  (cond
                    ((< (+ separator-sum sum) width)
                     (collect values :into values* :result-type vector)
                     (collect min    :into widths  :result-type vector)
                     (summing 1 :into count)
                     (summing (* priority (unweighted-score min)) :into score)
                     (unless (or (first-iteration-p) (equalp #(0) values))
                       (summing separator-width :into separator-sum)))
                    (t
                     (when (<= i 2)
                       (summing 1 :into count))
                     (let ((values (if (<= i 2) #(0) #())))
                       (collect values :into values* :result-type vector)
                       (collect 0      :into widths  :result-type vector)))))
                (finally
                 (return (values values* widths count score separator-sum)))))
         (unused (- width (reduce #'+ widths) separator-sum))
         ;; Available Monte Carlo moves.
         ((&flet move-1 (a old-a b old-b total)
            (declare (ignore old-a old-b))
            (let* ((new-a (random-elt-between
                           (aref values a) 0 total           random-state))
                   (new-b (random-elt-between
                           (aref values b) 0 (- total new-a) random-state)))
              (values new-a new-b))))
         ((&flet move-2 (a old-a b old-b total)
            (declare (ignore total))
            (let+ ((aux (if (zerop (random 2 random-state)) 1 -1))
                   ((&flet new-value (index old-value)
                      (let ((array (aref values index)))
                        (svref array
                               (clamp (+ (position old-value array :test #'=) aux)
                                      0 (1- (length array)))))))
                   (new-a (new-value a old-a))
                   (new-b (new-value b old-b)))
              (values new-a new-b))))
         ((&flet move-3 (a old-a b old-b total)
            (declare (ignore b total))
            (let ((array (aref values a))
                  (aux   (if (zerop (random 2 random-state)) 1 -1)))
              (values (svref array
                             (clamp (+ (position old-a array :test #'=) aux)
                                    0 (1- (length array))))
                      old-b)))))
    (declare (type (simple-vector) widths)
             (type (simple-array (simple-array fixnum (*)) (*)) values))
    ;; Simulated annealing with moves move-{1,2,3} for 10000
    ;; iterations.
    (loop :for temperature :of-type single-float = 100000.0f0 :then (* temperature .997f0)
       :for i :from 1 :below 10000 :do
       (let+ ((a     (random count random-state))
              (b     (random-but-not count a random-state))
              (old-a (svref widths a))
              (old-b (svref widths b))
              (total (+ old-a old-b unused))
              (move  (cond
                       ((< (random 10 random-state) 3)  #'move-2)
                       ((zerop (random 2 random-state)) #'move-1)
                       (t                               #'move-3)))
              ((&values new-a new-b) (funcall move a old-a b old-b total))
              (new-unused (+ unused old-a (- new-a) old-b (- new-b)))
              (change     (+ (* (- (unweighted-score new-a)
                                   (unweighted-score old-a))
                                (svref priorities a))
                             (* (- (unweighted-score new-b)
                                   (unweighted-score old-b))
                                (svref priorities b))
                             (* (1+ max-priority) 100000
                                (- (min 0 new-unused) (min 0 unused)))))
              (change*    (float (/ change temperature) 1.0f0)))
         (when (or (> change* (log 1))
                   (> (exp change*) (random 1.0f0 random-state)))
           (setf (svref widths a) new-a
                 (svref widths b) new-b
                 unused           new-unused
                 score            (+ score change)))))
    (values widths score)))

(defun optimize-widths (width specs priorities &key (separator-width 0))
  (unless (length= specs priorities)
    (error "~@<Widths and priorities have to sequences of equal ~
            length (widths: ~D, priorities: ~D)~@:>"
           (length specs) (length priorities)))
  (let+ ((length     (length specs))
         (difference (- 2 length))
         (fill       (when (plusp difference)
                       (make-list difference :initial-element 0)))
         (specs      (append specs fill))
         (priorities (append priorities fill))
         ((&values widths score)
          (%optimize-widths width specs priorities separator-width)))
    (values (if (plusp difference)
                (subseq widths 0 length)
                widths)
            score)))

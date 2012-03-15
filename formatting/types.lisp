;;; types.lisp --- Types used in the formatting module.
;;
;; Copyright (C) 2011, 2012 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This Program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This Program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses>.

(cl:in-package :rsb.formatting)

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


;;; Periodic printing
;;

(deftype print-interval ()
  "Print interval specifications."
  '(or null positive-real))


;;; Image-related types.
;;

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
;;

(deftype template-designator ()
  "A thing that can be converted into a formatting template."
  '(or string stream pathname))

(deftype script-designator ()
  "A thing that can be converted into a formatting script."
  '(or list string stream pathname))

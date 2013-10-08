;;;; types.lisp --- Types used in the stats module.
;;;;
;;;; Copyright (C) 2011, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.stats)

(deftype meta-data-selector ()
  "Either the name of a meta-data item or one of the special
names :keys and :values."
  '(or keyword (member :keys :values)))

(deftype when-missing-policy ()
  "Designator for a policy that should be employed if a meta-data item
is missing from an event. The keyword :skip causes the event to be
ignored. Strings are used as replacement values."
  '(or (eql :skip) string))

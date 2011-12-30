;;; types.lisp --- Types used in the stats module.
;;
;; Copyright (C) 2011 Jan Moringen
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

(cl:in-package :rsb.stats)

(deftype meta-data-selector ()
  "Either the name of a meta-data item or one of the special
names :keys and :values."
  '(or keyword (member :keys :values)))

(deftype when-missing-policy ()
  "Designator for a policy that should be employed if a meta-data item
is missing from an event. The keyword :skip causes the event to be
ignored. Strings are used as replacement values."
  '(or (eql :skip) string))

;;; style-compact.lisp ---
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

(cl:in-package :rsb.formatting.test)

(deftestsuite style-compact-root (formatting-root)
  ()
  (:documentation
   "Unit tests for the `style-compact' formatting style class."))

(addtest (style-compact-root
          :documentation
	  "Test some simple cases of formatting events using methods
on `format-event' for `style-compact'.")
  smoke

  (ensure-style-cases (style-compact)
    '(()
      ()
      "")

    `((:header-frequency nil)
      (,(make-event "/foo" "bar"))
      ".*…\\|ORIGIN\\? \\|/foo/           \\|\"bar\"           \\|        3
")

    `((:header-frequency nil)
      (,(make-event "/foo" "bar") ,(make-event "/fez" "whoop"))
      ".*…\\|ORIGIN\\? \\|/foo/           \\|\"bar\"           \\|        3
.*…\\|ORIGIN\\? \\|/fez/           \\|\"whoop\"         \\|        5
")))

(deftestsuite style-compact+-root (formatting-root)
  ()
  (:documentation
   "Unit tests for the `style-compact+' formatting style class."))

(addtest (style-compact+-root
          :documentation
	  "Test some simple cases of formatting events using methods
on `format-event' for `style-compact+'.")
  smoke

  (ensure-style-cases (style-compact+)
    '(()
      ()
      "")

    `((:header-frequency nil)
      (,(make-event "/foo" "bar"))
      ".*\\|ORIGIN\\? \\|     NIL\\|EVENTID…\\|<nomethod>\\|/foo/                   \\|\"bar\"                \\|        3
")

    `((:header-frequency nil)
      (,(make-event "/foo" "bar") ,(make-event "/fez" "whoop"))
      ".*\\|ORIGIN\\? \\|     NIL\\|EVENTID…\\|<nomethod>\\|/foo/                   \\|\"bar\"                \\|        3
.*\\|ORIGIN\\? \\|     NIL\\|EVENTID…\\|<nomethod>\\|/fez/                   \\|\"whoop\"              \\|        5
")))

;; Local Variables:
;; coding: utf-8
;; End:

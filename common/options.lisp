;;; options.lisp --- Common functions related to commandline options.
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

(in-package :rsb.common)

(defun make-common-options ()
  "Return a `clon:group' instance containing common program options."
  (defgroup (:header "General Options")
    (flag :short-name     "v"
	  :long-name      "version"
	  :description
	  "Print version information exit.")
    (flag :short-name     "h"
	  :long-name      "help"
	  :description
	  "Print this help and exit.")
    (flag :short-name     "d"
	  :long-name      "debug"
	  :description
	  "Enable debugging.")))

(defun process-commandline-options (&key
				    (version '(0 1 0))
				    update-synopsis
				    return)
  "Perform the following commandline option processing:

+ if --debug has been supplied, keep debugger enabled and adjust
  log-level, otherwise disable debugger

+ if --version has been supplied, print version information and call
  RETURN or exit.

+ if --help has been supplied, print a help text and call RETURN or
  exit."
  ;; Create a new global context.
  (make-context)

  (if (getopt :long-name "debug")
      (log5:debugging 'log5:info+)
      (disable-debugger))

  ;; Load specified RSB plugins, potentially updating the option
  ;; synopsis afterwards.
  ;; (rsb::load-plugins)
  ;; (when update-synopsis
  ;;   (funcall update-synopsis)
  ;;
  ;;   ;; Create a new global context.
  ;;   (make-context))

  (when (getopt :long-name "version")
    (print-version version *standard-output*
		   :include-rsb-version? t)
    (terpri *standard-output*)
    (if return
	(funcall return)
	(exit 0)))

  (when (getopt :long-name "help")
    (help)
    (if return
	(funcall return)
	(exit 0))))

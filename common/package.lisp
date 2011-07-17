;;; package.lisp --- Package definition for common module.
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

(in-package :cl-user)

(defpackage :rsb.common
  (:use
   :cl
   :alexandria
   :bind
   :iterate

   :com.dvlsoft.clon

   :rsb)

  ;; Conditions
  (:export
   :failed-to-load-idl
   :failed-to-load-idl-source)

  ;; Filter spec parsing and filter construction
  (:export
   :parse-filter-spec
   :make-filter)

  ;; IDL loading
  (:export
   :load-idl)

  ;; Logging
  (:export
   :with-logged-warnings)

  ;; Commandline options
  (:export
   :make-common-options
   :process-commandline-options)

  ;; Interactive stuff
  (:export
   :with-interactive-interrupt-exit)

  ;; Help text generation
  (:export
   :print-uri-help
   :print-filter-help

   :print-version)

  ;; Debugging
  (:export
   :trace-things
   :disable-debugger

   :start-swank
   :enable-swank-on-signal)

  (:documentation
   "This package contains some common utility functions for RSB:
+ Commandline option definition and processing
+ Help text generation
+ Debugger control"))

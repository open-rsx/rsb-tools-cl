;;;; idl-loading.lisp --- Unit tests for IDL loading functionality.
;;;;
;;;; Copyright (C) 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.tools.common.test)

(deftestsuite common-idl-loading-root (common-root)
  ()
  (:documentation
   "Test suite for IDL loading functionality."))

(addtest (common-idl-loading-root
          :doumentation
          "Smoke test for the `find-and-load-idl' function.")
  find-and-load-idl/smoke

  (ensure-condition error
    (find-and-load-idl ".some.Name" :proto))

  (let* ((data-directory        (asdf:system-relative-pathname
                                 :rsb-tools-common-test "test/data/"))
         (pbf:*proto-load-path* (list data-directory)))
    (find-and-load-idl ".test.Simple" :proto)))

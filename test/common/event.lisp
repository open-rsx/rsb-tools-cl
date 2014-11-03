;;;; event.lisp --- Unit tests for parsing events and payloads.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.common.test)

(deftestsuite common-event-root (common-root)
  ()
  (:documentation
   "Test suite for event and payload parsing functions."))

(addtest (common-event-root
          :documentation
          "Smoke test for the `parse-call-spec' function.")
  parse-call-spec/smoke

  (ensure-cases (input expected-uri &optional expected-method expected-arg)
      `(;; Some invalid cases
        (""                        call-specificiation-error)
        ("/"                       call-specificiation-error)
        ("("                       call-specificiation-error)
        ("()"                      call-specificiation-error)
        ("/("                      call-specificiation-error)
        ("/()"                     call-specificiation-error)

        ;; These are OK.
        ("/foo()"                  ,(rsb:make-scope "/")           "foo" ,rsb.converter:+no-value+)
        ("/foo/bar()"              ,(rsb:make-scope "/foo")        "bar" ,rsb.converter:+no-value+)
        ("/foo/bar/baz()"          ,(rsb:make-scope "/foo/bar")    "baz" ,rsb.converter:+no-value+)
        ("socket:/foo()"           ,(puri:uri "socket:")           "foo" ,rsb.converter:+no-value+)
        ("socket:/foo/bar()"       ,(puri:uri "socket:/foo")       "bar" ,rsb.converter:+no-value+)
        ("socket:/foo/bar/baz()"   ,(puri:uri "socket:/foo/bar")   "baz" ,rsb.converter:+no-value+)
        ("/?server=1/foo()"        ,(puri:uri "/?server=1")        "foo" ,rsb.converter:+no-value+)
        ("/foo?server=1/bar()"     ,(puri:uri "/foo?server=1")     "bar" ,rsb.converter:+no-value+)
        ("/foo/bar?server=1/baz()" ,(puri:uri "/foo/bar?server=1") "baz" ,rsb.converter:+no-value+)

        ;; Different payload types.
        ("/foo()"                  ,(rsb:make-scope "/")           "foo" ,rsb.converter:+no-value+)
        ("/foo(false)"             ,(rsb:make-scope "/")           "foo" nil)
        ("/foo(true)"              ,(rsb:make-scope "/")           "foo" t)
        ("/foo(1)"                 ,(rsb:make-scope "/")           "foo" 1)
        ("/foo(.1)"                ,(rsb:make-scope "/")           "foo" .1)
        ("/foo(\"bar\")"           ,(rsb:make-scope "/")           "foo" "bar"))

    (let+ (((&flet do-it () (parse-call-spec input)))
           ((&flet scope-or-uri= (left right)
              (or (and (typep left 'rsb:scope) (typep right 'rsb:scope)
                       (rsb:scope= left right))
                  (and (typep left 'puri:uri) (typep right 'puri:uri)
                       (puri:uri= left right))))))
     (case expected-uri
       (call-specificiation-error
        #+no (ensure-condition 'call-specificiation-error (do-it)))
       (t
        (let+ (((&values server-uri method arg) (do-it)))
          (ensure-same server-uri expected-uri    :test #'scope-or-uri=)
          (ensure-same method     expected-method :test #'string=)
          (ensure-same arg        expected-arg    :test #'equalp)))))))

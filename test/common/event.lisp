;;;; event.lisp --- Unit tests for parsing events and payloads.
;;;;
;;;; Copyright (C) 2014, 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.tools.common.test)

(deftestsuite common-event-root (common-root)
  ()
  (:documentation
   "Test suite for event and payload parsing functions."))

(addtest (common-event-root
          :doumentation
          "Smoke test for the `parse-payload-spec' function")
  parse-payload-spec/smoke

  (let+ ((pathname      (asdf:system-relative-pathname
                         :rsb-tools-common
                         #P"test/data/simple.protocol-buffer-text"))
         (a-file        (format nil "~S" pathname))
         (a-utf-8-file  (format nil "~S:utf-8" pathname))
         (a-binary-file (format nil "~S:binary" pathname))
         (a-pb-payload  "pb:.rsb.protocol.Notification:{data:\"foo\"}")
         ((&flet pb-file (namestring)
            (format nil "pb:.rsb.protocol.Notification:~A" namestring))))
    (ensure-cases (input expected-payload)
        `( ;; Some invalid cases
          ("("                            error)
          ("/("                           error)
          ("/()"                          error)
          ("pb:.foo.Bar:#P\"foo\":binary" error)
          ("pb:.foo.Bar:-:binary"         error)
          ("1 \"foo\""                    error)

          ;; Different payload types.
          (""                             ,rsb.converter:+no-value+)
          ("false"                        nil)
          ("true"                         t)
          ("/bar"                         ,(rsb:make-scope "/bar"))
          (,a-file                        ,(read-file-into-string pathname))
          (,a-utf-8-file                  ,(read-file-into-string pathname))
          (,a-binary-file                 ,(read-file-into-byte-vector pathname))
          (,a-pb-payload                  rsb.protocol:notification)
          (,(pb-file a-file)              rsb.protocol:notification)
          (,(pb-file a-utf-8-file)        rsb.protocol:notification)
          ("1"                            1)
          (".1"                           .1)
          ("\"bar\""                      "bar"))

      (let+ (((&flet do-it () (parse-payload-spec input)))
             ((&flet scope=-or-equalp (left right)
                (or (and (typep left 'rsb:scope) (typep right 'rsb:scope)
                         (rsb:scope= left right))
                    (equalp left right)))))
        (case expected-payload
          (error
           (ensure-condition 'error (do-it)))
          (rsb.protocol:notification
           (ensure (typep (do-it) 'rsb.protocol:notification)))
          (t
           (ensure-same (do-it) expected-payload
                        :test #'scope=-or-equalp)))))))

(addtest (common-event-root
          :documentation
          "Smoke test for the `parse-call-spec' function.")
  parse-call-spec/smoke

  (ensure-cases (input expected-uri &optional expected-method expected-arg)
      `(;; Some invalid cases
        (""                           call-specificiation-error)
        ("/"                          call-specificiation-error)
        ("("                          call-specificiation-error)
        ("()"                         call-specificiation-error)
        ("/("                         call-specificiation-error)
        ("/()"                        call-specificiation-error)

        ;; These are OK.
        ("/foo()"                     ,(rsb:make-scope "/")              "foo" ,rsb.converter:+no-value+)
        ("/foo/bar()"                 ,(rsb:make-scope "/foo")           "bar" ,rsb.converter:+no-value+)
        ("/foo/bar/baz()"             ,(rsb:make-scope "/foo/bar")       "baz" ,rsb.converter:+no-value+)
        ("socket:/foo()"              ,(puri:uri "socket:")              "foo" ,rsb.converter:+no-value+)
        ("socket:/foo/bar()"          ,(puri:uri "socket:/foo")          "bar" ,rsb.converter:+no-value+)
        ("socket:/foo/bar/baz()"      ,(puri:uri "socket:/foo/bar")      "baz" ,rsb.converter:+no-value+)
        ("socket://foo/bar/baz()"     ,(puri:uri "socket://foo/bar")     "baz" ,rsb.converter:+no-value+)
        ("socket://1.2.3.4/bar/baz()" ,(puri:uri "socket://1.2.3.4/bar") "baz" ,rsb.converter:+no-value+)
        #+later ("socket://[::]/bar/baz()"    ,(puri:uri "socket://[::]/bar")    "baz" ,rsb.converter:+no-value+)
        ("/?server=1/foo()"           ,(puri:uri "/?server=1")           "foo" ,rsb.converter:+no-value+)
        ("/foo?server=1/bar()"        ,(puri:uri "/foo?server=1")        "bar" ,rsb.converter:+no-value+)
        ("/foo/bar?server=1/baz()"    ,(puri:uri "/foo/bar?server=1")    "baz" ,rsb.converter:+no-value+)

        ;; Different payload types.
        ("/foo()"                     ,(rsb:make-scope "/")              "foo" ,rsb.converter:+no-value+)
        ("/foo(false)"                ,(rsb:make-scope "/")              "foo" nil)
        ("/foo(true)"                 ,(rsb:make-scope "/")              "foo" t)
        ("/foo(/bar)"                 ,(rsb:make-scope "/")              "foo" ,(rsb:make-scope "/bar"))
        ("/foo(1)"                    ,(rsb:make-scope "/")              "foo" 1)
        ("/foo(.1)"                   ,(rsb:make-scope "/")              "foo" .1)
        ("/foo(\"bar\")"              ,(rsb:make-scope "/")              "foo" "bar"))

    (let+ (((&flet do-it () (parse-call-spec input)))
           ((&flet scope=-or-uri=-or-equalp (left right)
              (or (and (typep left 'rsb:scope) (typep right 'rsb:scope)
                       (rsb:scope= left right))
                  (and (typep left 'puri:uri) (typep right 'puri:uri)
                       (puri:uri= left right))
                  (equalp left right)))))
     (case expected-uri
       (call-specificiation-error
        (ensure-condition 'call-specification-error (do-it)))
       (t
        (let+ (((&values server-uri method arg) (do-it)))
          (ensure-same server-uri expected-uri
                       :test #'scope=-or-uri=-or-equalp)
          (ensure-same method     expected-method :test #'string=)
          (ensure-same arg        expected-arg
                       :test #'scope=-or-uri=-or-equalp)))))))

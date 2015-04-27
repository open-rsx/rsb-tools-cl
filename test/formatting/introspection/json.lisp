;;;; json.lisp --- Unit tests for the JSON-serialization of introspection data.
;;;;
;;;; Copyright (C) 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.formatting.introspection.test)

(define-constant +configuration-only-inprocess+
    '(((:transport :inprocess :enabled) . "1")
      ((:transport :socket :enabled)    . "0")
      ((:transport :spread :enabled)    . "0")
      ((:introspection :enabled)        . "1"))
  :test #'equal)

(deftestsuite rsb.formatting.introspection.json-root
    (rsb.formatting.introspection-root)
  ()
  (:documentation
   "Unit tests for the JSON-serialization of introspection data."))

(addtest (rsb.formatting.introspection.json-root
          :documentation
          "Smoke test for the JSON-serialization of introspection
           data.")
  smoke

  (let ((*configuration* +configuration-only-inprocess+)
        (transports      '((:inprocess :enabled t) (t :enabled nil))))
    (with-participants ((introspection :remote-introspection
                                       rsb.introspection:+introspection-scope+
                                       :receiver-uris '("/"))
                        (listener :listener "/"))
      (json:decode-json-from-string
       (json:encode-json-to-string introspection)))))

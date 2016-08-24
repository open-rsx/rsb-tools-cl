;;;; json.lisp --- Unit tests for the JSON-serialization of introspection data.
;;;;
;;;; Copyright (C) 2015, 2016 Jan Moringen
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

  (let ((*configuration* +configuration-only-inprocess+))
    (with-participants ((introspection :remote-introspection
                                       rsb.introspection:+introspection-scope+
                                       :receiver-uris '("/"))
                        (nil           :listener "/"))
      (let ((style (make-style
                    :json
                    :database (rsb.introspection::introspection-database
                               introspection)
                    :service  'rsb.formatting.introspection::style)))
        (json:decode-json-from-string
         (with-output-to-string (stream)
           (rsb.introspection:with-database-lock (introspection)
             (format-event :dummy style stream))))))))

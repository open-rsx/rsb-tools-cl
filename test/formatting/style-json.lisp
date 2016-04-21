;;;; style-json.lisp --- Unit tests for the JSON formatting style.
;;;;
;;;; Copyright (C) 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.formatting.test)

(deftestsuite style-json-root (formatting-root)
  ()
  (:documentation
   "Unit tests for the `style-json' formatting style class."))

(addtest (style-json-root
          :documentation
          "Test some simple cases of formatting events using methods
           on `format-event' for `style-json'.")
  smoke

  (ensure-style-cases (style-json)
    ;; Some payloads.
    `(()
      (,(make-event "/foo" "bar"))
      ,(format nil "{\"scope\":\"\\\\/foo\\\\/\",\"method\":null,\"metaData\":{},~
                     \"timestamp\":{\"create\":\"[^\"]+\"},\"cause\":\\[],~
                     \"data\":\"bar\"}"))
    `(()
      (,(make-event "/foo" 1))
      ,(format nil "{\"scope\":\"\\\\/foo\\\\/\",\"method\":null,\"metaData\":{},~
                     \"timestamp\":{\"create\":\"[^\"]+\"},\"cause\":\\[],~
                     \"data\":1}"))
    `(()
      (,(make-event "/foo" (make-instance 'rsb.protocol:notification)))
      ,(format nil "{\"scope\":\"\\\\/foo\\\\/\",\"method\":null,\"metaData\":{},~
                     \"timestamp\":{\"create\":\"[^\"]+\"},\"cause\":\\[],~
                     \"data\":{\"scope\":\\[],\"method\":\\[],\"wireSchema\":\\[],~
                     \"data\":\\[],\"eventId\":{\"senderId\":\\[],\"sequenceNumber\":0},~
                     \"causes\":\\[],\"metaData\":{\"createTime\":0,\"sendTime\":0,~
                     \"receiveTime\":0,\"deliverTime\":0,\"userTimes\":\\[],~
                     \"userInfos\":\\[]}}}"))
    ;; Meta-data
    `(()
      (,(make-event "/foo" 1 :method :|method|))
      ,(format nil "{\"scope\":\"\\\\/foo\\\\/\",\"method\":\"method\",\"metaData\":{},~
                     \"timestamp\":{\"create\":\"[^\"]+\"},\"cause\":\\[],~
                     \"data\":1}"))
    `(()
      (,(make-event "/foo" 1 :foo "bar"))
      ,(format nil "{\"scope\":\"\\\\/foo\\\\/\",\"method\":null,~
                     \"metaData\":{\"foo\":\"bar\"},~
                     \"timestamp\":{\"create\":\"[^\"]+\"},\"cause\":\\[],~
                     \"data\":1}"))
    `(()
      (,(make-event "/foo" 1 :timestamps `(:baz ,(local-time:now))))
      ,(format nil "{\"scope\":\"\\\\/foo\\\\/\",\"method\":null,\"metaData\":{},~
                     \"timestamp\":{\"create\":\"[^\"]+\",\"baz\":\"[^\"]+\"},~
                     \"cause\":\\[],~
                     \"data\":1}"))
    `(()
      (,(make-event "/foo" 1 :causes `(,(cons (uuid:make-null-uuid) 0))))
      ,(format nil "{\"scope\":\"\\\\/foo\\\\/\",\"method\":null,\"metaData\":{},~
                     \"timestamp\":{\"create\":\"[^\"]+\"},~
                     \"cause\":\\[\"2583F0ED-4C6A-59D3-A061-AD9AF50616C6\"],~
                     \"data\":1}"))))

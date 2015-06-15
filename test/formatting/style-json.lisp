;;;; style-json.lisp --- Unit tests for the JSON formatting style.
;;;;
;;;; Copyright (C) 2015 Jan Moringen
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
                     \"timestamps\":{\"create\":\"[^\"]+\"},\"causes\":\\[],~
                     \"data\":\"bar\"}"))
    `(()
      (,(make-event "/foo" 1))
      ,(format nil "{\"scope\":\"\\\\/foo\\\\/\",\"method\":null,\"metaData\":{},~
                     \"timestamps\":{\"create\":\"[^\"]+\"},\"causes\":\\[],~
                     \"data\":1}"))
    `(()
      (,(make-event "/foo" (make-instance 'rsb.protocol:notification)))
      ,(format nil "{\"scope\":\"\\\\/foo\\\\/\",\"method\":null,\"metaData\":{},~
                     \"timestamps\":{\"create\":\"[^\"]+\"},\"causes\":\\[],~
                     \"data\":{\"eventId\":{\"senderId\":\\[],\"sequenceNumber\":0},~
                     \"scope\":\\[],\"method\":\\[],\"wireSchema\":\\[],\"data\":\\[],~
                     \"causes\":\\[],\"metaData\":{\"createTime\":0,\"sendTime\":0,~
                     \"receiveTime\":0,\"deliverTime\":0,\"userTimes\":\\[],~
                     \"userInfos\":\\[]}}}"))
    ;; Meta-data
    `(()
      (,(make-event "/foo" 1 :method :|method|))
      ,(format nil "{\"scope\":\"\\\\/foo\\\\/\",\"method\":\"method\",\"metaData\":{},~
                     \"timestamps\":{\"create\":\"[^\"]+\"},\"causes\":\\[],~
                     \"data\":1}"))
    `(()
      (,(make-event "/foo" 1 :foo "bar"))
      ,(format nil "{\"scope\":\"\\\\/foo\\\\/\",\"method\":null,~
                     \"metaData\":{\"foo\":\"bar\"},~
                     \"timestamps\":{\"create\":\"[^\"]+\"},\"causes\":\\[],~
                     \"data\":1}"))
    `(()
      (,(make-event "/foo" 1 :timestamps `(:baz ,(local-time:now))))
      ,(format nil "{\"scope\":\"\\\\/foo\\\\/\",\"method\":null,\"metaData\":{},~
                     \"timestamps\":{\"create\":\"[^\"]+\",\"baz\":\"[^\"]+\"},~
                     \"causes\":\\[],~
                     \"data\":1}"))
    `(()
      (,(make-event "/foo" 1 :causes `(,(cons (uuid:make-null-uuid) 0))))
      ,(format nil "{\"scope\":\"\\\\/foo\\\\/\",\"method\":null,\"metaData\":{},~
                     \"timestamps\":{\"create\":\"[^\"]+\"},~
                     \"causes\":\\[\"2583F0ED-4C6A-59D3-A061-AD9AF50616C6\"],~
                     \"data\":1}"))))

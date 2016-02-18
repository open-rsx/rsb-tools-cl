;;;; event-style-bridge.lisp --- TODO.
;;;;
;;;; Copyright (C) 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.tools.bridge)

(defclass event-style-bridge (rsb.formatting::output-buffering-mixin
                              rsb.formatting:periodic-printing-mixin
                              rsb.formatting:sub-style-grouping-mixin
                              rsb.formatting::widths-caching-mixin
                              rsb.formatting:separator-mixin)
  ((sub-style-spec :initarg  :sub-style-spec
                   :reader   style-sub-style-spec)
   (connections    :accessor style-connections
                   :initform '()))
  (:default-initargs
   :key       #'car
   :test      #'eq
   :separator :clear))

(defmethod rsb.formatting:delegate ((event  cons)
                                    (style  event-style-bridge)
                                    (stream t))
  ;; EVENT is (CONNECTION . ACTUAL-EVENT)
  (let ((actual-event (cdr event))
        (sub-style    (first (rsb.formatting:sub-style-for style event))))
    (rsb.formatting:format-event actual-event sub-style stream)))

(defmethod rsb.formatting:make-sub-style-entry ((style event-style-bridge)
                                                (value t))
  (let+(((&accessors-r/o (key            rsb.formatting:style-key)
                         (test           rsb.formatting:style-test)
                         (sub-style-spec style-sub-style-spec))
         style))
    #+todo (declare (type function key test))
    (push value (style-connections style))
    (cons (lambda (event) (funcall test (funcall key event) value))
          (rsb.formatting:make-style (append (ensure-list sub-style-spec)
                                             (list :print-interval nil
                                                   :separator      nil))))))

(defmethod rsb.formatting:format-event ((event  (eql :trigger))
                                        (style  event-style-bridge)
                                        (target t)
                                        &key
                                        max-lines max-columns)
  (let ((*print-right-margin* (when *print-right-margin* (- *print-right-margin* 2)))) ; TODO
    (mapc (lambda (connection)
            (let ((style (first (rsb.formatting:sub-style-for
                                 style (cons connection nil)))))
              (format target "~/print-items:format-print-items/~2%"
                      (connection-print-items connection))
              (pprint-logical-block (target nil :per-line-prefix "  ")
                (rsb.formatting:format-event :trigger style target))
              (format target "~%")))
          (style-connections style))))

;;; Utilities

(defun connection-print-items (object)
  (let+ (((&flet simplify-url (uri)
            (puri:copy-uri uri :query nil :fragment nil)))
         ((&flet urls-of-type (type)
            (let* ((children (rsb.patterns:participant-children object))
                   (matching (remove-if-not (of-type type) children)))
              (mapcar #'simplify-url
                      (mapcan #'rsb:transport-specific-urls matching)))))
         (inputs   (urls-of-type 'listener))
         (outputs  (urls-of-type 'informer)))
    `((:inputs  ,inputs  "~{~A~^, ~}" ((:before :arrow)))
      (:arrow   nil      " → ")
      (:outputs ,outputs "~{~A~^, ~}" ((:after :arrow))))))

(defun make-hook (sub-style stream)
  (let+ ((style (make-instance 'event-style-bridge
                               :sub-style-spec sub-style))
         ((&flet hook (connection event)
            (rsb.formatting:format-event
             (cons connection event) style stream))))
    #'hook))

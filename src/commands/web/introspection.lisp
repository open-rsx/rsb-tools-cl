;;;; introspection.lisp --- Serve introspection information over HTTP.
;;;;
;;;; Copyright (C) 2014, 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.tools.commands.web)

;;; `introspection-handler-mixin'

(defclass introspection-handler-mixin (handler-mixin)
  ((database :initarg  :database
             :reader   handler-database))
  (:default-initargs
   :database (missing-required-initarg 'introspection-handler-mixin :database))
  (:documentation
   "This class is intended to be mixed into handler classes that serve
    introspection information."))

;;; `introspection-snapshot-handler'

(defclass introspection-snapshot-handler (introspection-handler-mixin)
  ()
  (:metaclass closer-mop:funcallable-standard-class)
  (:documentation
   "Instances of this class serve JSON-serialized introspection
    information."))

(defun make-introspection-snapshot-handler (database)
  (values "/api/introspection/snapshot"
          (make-instance 'introspection-snapshot-handler :database database)))

(pushnew 'make-introspection-snapshot-handler *default-handlers*)

(defmethod rsb.ep:handle ((sink introspection-snapshot-handler)
                          (data hunchentoot:request))
  (providing-api-endpoint (:request data) ()
    "Replies with a snapshot of the available introspection information.

     The introspection snapshot is structured as a tree with the root
     containing host nodes, host nodes containing process nodes and
     process nodes containing trees of nested participant nodes."
    (lambda (stream)
      (let ((database (handler-database sink)))
        (rsb.introspection:with-database-lock (database)
          (let ((tree (rsb.introspection::introspection-database database)))
            (architecture.builder-protocol.json:serialize-using-serializer
             t tree stream (default-json-serializer))))))))

;;; `introspection-search-handler'

(defclass introspection-search-handler (introspection-handler-mixin)
  ()
  (:metaclass closer-mop:funcallable-standard-class)
  (:documentation
   "Instances of this class serve results of search queries on
    introspection information."))

(defun make-introspection-search-handler (database)
  (values "/api/introspection/search"
          (make-instance 'introspection-search-handler :database database)))

(pushnew 'make-introspection-search-handler *default-handlers*)

(defun search-parse-query (query)
  (let+ (((&flet whitespace? (character)
            (member character '(#\Space #\Tab #\Newline))))
         ((&flet wordoid? (character)
            (or (alphanumericp character) (find character "-_"))))
         (words (when (every (disjoin #'wordoid? #'whitespace?) query)
                  (split-sequence:split-sequence-if
                   #'whitespace? query :remove-empty-subseqs t)))
         ((&flet word-query (word)
            `(:attribute * (:contains (:path (:self :node)) ,word))))
         ((&flet word-query/path (word)
            `(:path ,(word-query word))))
         (expression
          (cond
            ;; Only whitespace.
            ((every #'whitespace? query)
             (argument-error 'query "~@<No search term ~
                                     specified (query has only ~
                                     whitespace characters).~@:>"))
            ;; One or more "words".
            ((not (emptyp words))
             `(:path (:root :node)
                     (:descendant-or-self :node)
                     ,(if (length= 1 words)
                          (word-query (first words))
                          `(:child * (:and ,@(mapcar #'word-query/path
                                                     words))))))
            ;; XPath expression.
            (t
             (xpath:parse-xpath query)))))
    `(xpath:xpath ,expression)))

(defun search-result-formatter (result navigator &key start count)
  (etypecase result
    ((or string number)
     (when (and start (plusp start))
       (argument-error 'start "~@<Positive start index but atom result.~@:>"))
     (lambda (stream)
       (json:encode-json result stream)))
    (xpath:node-set
     ;; Drop requested number of elements from the pipe.
     (when (and start (plusp start))
       (let ((xpath:*navigator* navigator))
         (loop :with pipe = (xpath-sys:pipe-of result)
            :for i :from 0
            :repeat start
            :when (xpath::pipe-empty-p pipe)
            :do (argument-error
                 'start "~@<Start element ~:D requested, but result only ~
                       has ~:D element~:P.~@:>"
                 start i)
            :do (setf pipe (xpath-sys:pipe-tail pipe))
            :finally (setf (xpath-sys:pipe-of result) pipe))))
     ;; Serialize requested number of nodes into the reply.
     (let ((count (when count (1+ count))))
       (lambda (stream)
         (let ((xpath:*navigator* navigator))
           (json:with-array (stream)
             (xpath:do-node-set (node result)
               (when (and count (zerop (decf count)))
                 (return))
               (let+ (((&flet unwrap (thing)
                         (architecture.builder-protocol.xpath:unwrap
                          navigator thing)))
                      ((&flet parent ()
                         (architecture.builder-protocol.xpath::proxy-parent node)))
                      ((&values attribute element)
                       (typecase node
                         (architecture.builder-protocol.xpath::node-proxy
                          (values nil           (unwrap node)))
                         (architecture.builder-protocol.xpath::attribute-proxy
                          (values (unwrap node) (unwrap (parent))))
                         (architecture.builder-protocol.xpath::relation-proxy
                          (values nil           (unwrap (parent)))))))
                 (when (or attribute element)
                   (json:as-array-member (stream)
                     (json:with-object (stream)
                       (when attribute
                         (let ((name  (string-downcase (car attribute)))
                               (value (rsb.formatting::prepare-initarg-value-for-json
                                       (cdr attribute))))
                           (json:encode-object-member "attribute-name"  name  stream)
                           (json:encode-object-member "attribute-value" value stream)))
                       (when element
                         (json:as-object-member ("element" stream)
                           (architecture.builder-protocol.json:serialize-using-serializer
                            :reverse element stream (default-json-serializer))))))))))))))))

(defmethod rsb.ep:handle ((sink introspection-search-handler)
                          (data hunchentoot:request))
  (providing-api-endpoint (:request data)
      ((query                                          :transform 'search-parse-query
        :documentation
        "The (non-empty) query string.

         One of the following things:

         An XPath expression.

         One or more words: match any node (element, attribute, text)
         containing all words.")
       &optional
       (start               :type non-negative-integer :transform 'parse-integer
        :documentation
        "Index of first node in match sequence that should be returned.")
       (count :name "limit" :type positive-integer     :transform 'parse-integer
        :documentation
        "Number of nodes from the match sequence that should be returned."))
    "Run XPath queries against the available introspection information.

     Return an atomic result for expressions not evaluating to node
     sets and an array of matches otherwise. An atomic result can be a
     number or string. For example, the result of the query

       count(//@foo)

     is a number. A match can be an attribute match or an element
     match."
    (let ((navigator (make-instance
                       'architecture.builder-protocol.xpath:navigator
                       :builder       t
                       :peek-function (lambda (builder relation relation-args node)
                                        (declare (ignore builder relation relation-args))
                                        (typecase node
                                          (rsb.formatting::stringify-value
                                           (rsb.formatting::maybe-stringify-value node))
                                          (number
                                           (princ-to-string node))
                                          (string
                                           node)
                                          (t
                                           t)))
                       :printers      `((,(lambda (builder node)
                                            (declare (ignore builder))
                                            (typep node 'rsb.formatting::stringify-value))
                                         . ,(lambda (builder node)
                                              (declare (ignore builder))
                                              (rsb.formatting::maybe-stringify-value node))))))
          (database  (handler-database sink)))
      (rsb.introspection:with-database-lock (database)
        (let* ((tree   (rsb.introspection::introspection-database database))
               (result (architecture.builder-protocol.xpath:evaluate-using-navigator
                        query navigator tree :node-order nil)))
          ;; Return a closure that will be called with the reply
          ;; stream.
          (search-result-formatter result navigator
                                   :start start :count count))))))

;;; Utilities

(defun default-json-serializer ()
  (rsb.formatting::make-json-serializer
   :symbol-transform #'string-downcase
   :kind-transform   (architecture.builder-protocol.json:default-kind-key-and-value-transform)
   :peek-function    (rsb.formatting::make-json-peek-function)))

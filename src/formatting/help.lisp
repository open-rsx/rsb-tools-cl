;;;; help.lisp --- Help text generation for formatting options.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014, 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.formatting)

(defun make-style-service-help-string (&key
                                       (service          'style)
                                       initarg-blacklist)
  (with-output-to-string (stream)
    (format stream "The following formatting styles are currently ~
                    available:~@
                    ~@
                    ")
    (let* ((providers (service-provider:service-providers service))
           (classes   (mapcar (lambda (provider)
                                (list
                                 (service-provider:provider-name provider)
                                 (service-provider:provider-class provider)))
                              providers)))
      (rsb.tools.common:print-classes-help-string
       classes stream :initarg-blacklist initarg-blacklist))))

(defun make-style-help-string (&key
                               (show :default))
  "Return a help string that explains how to specify a formatting
   style and its parameters."
  (with-output-to-string (stream)
    (format stream "Specify a formatting style that should be used to ~
                    print events. SPEC has to be of the form~@
                    ~@
                    ~2@TKIND KEY1 VALUE1 KEY2 VALUE2 ...~@
                    ~@
                    where keys and values are optional and depend on ~
                    KIND. Examples (note that the single quotes have ~
                    to be included only when used within a shell):~@
                    ~@
                    ~2@T--style detailed~@
                    ~2@T--style compact~@
                    ~2@T--style 'compact :separator \"|\"'~@
                    ~2@T--style 'columns :columns (:now (:scope :width 12) :id :newline)'~@
                    ~2@T\(see extended help, enable with ~
                      --help-for=columns, for an explanation of ~
                      the :columns argument\)~@
                    ~@
                    ")
    (rsb.tools.common:with-abbreviation
        (stream '(:styles :columns :quantities) show)
      (write-string
       (make-style-service-help-string
        :initarg-blacklist '(:stream :pretty-state
                             :quantities :count
                             :sub-styles :test :key
                             :sort-predicate :sort-key
                             :code
                             :builder
                             :event-peek-function
                             :payload-peek-function))
       stream)
      (format stream "~%~%")
      (rsb.tools.common:with-abbreviation (stream :columns show)
        (format stream "In column-based formatting styles, columns can ~
                        be selected and configured using the :columns ~
                        argument and a syntax of the form~@
                        ~@
                        ~2@T:columns (COLSPEC1 COLSPEC2 ...)~@
                        ~@
                        where~@
                        ~@

                        ~2@TCOLSPEC ::= KIND | (KIND KEY1 VALUE1 KEY2 ~
                          VALUE2 ...)~@
                        ~@
                        The following columns are available:~@
                        ~@
                        ")
        (rsb.tools.common:print-classes-help-string
         (mapcar (lambda (provider)
                   (list (service-provider:provider-name provider)
                         (service-provider:provider-class provider)))
                 (service-provider:service-providers 'column))
         stream))

      (when-let* ((package   (find-package :rsb.stats))
                  (symbol    (find-symbol "QUANTITY" :rsb.stats))
                  (providers (service-provider:service-providers symbol))
                  (classes   (mapcar (lambda (provider)
                                       (list
                                        (service-provider:provider-name provider)
                                        (service-provider:provider-class provider)))
                                     providers)))
        (format stream "~%~%")
        (rsb.tools.common:with-abbreviation (stream :quantities show)
          (format stream "In the statistics style, statistical ~
                          quantities are used in columns. These ~
                          columns can be configured using the :columns ~
                          argument and a syntax of the form~@
                          ~@
                          ~2@T:columns (COLSPEC1 COLSPEC2 ...)~@
                          ~@
                          where~@
                          ~@

                          ~2@TCOLSPEC      ::= (:quantity :quantity ~
                            QUANTITYSPEC KEY1 VALUE1 KEY2 VALUE2 ...)~@
                          ~2@TQUANTITYSPEC ::= KIND | (KIND KEY1 VALUE1 ~
                            KEY2 VALUE2 ...)~@
                          ~@
                          The following quantities are available:~@
                          ~@
                          ")
          (rsb.tools.common:print-classes-help-string
           classes stream
           :initarg-blacklist '(:extractor :reduce-by :start-time :values)))))))

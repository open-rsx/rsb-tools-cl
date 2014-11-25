;;;; help.lisp --- Automatic generation of help strings.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.common)

;;; Selective help display

(defun show-help-for? (category
                       &key
                       default
                       show)
  "Return non-nil if help for CATEGORY should be shown.
   SHOW contains the specification of which help to show.  DEFAULT
   controls the result if SHOW is :default."
  (case show
    ((t)      t)
    (:default default)
    (t        (intersection (ensure-list category)
                            (ensure-list show)))))

(defmacro with-abbreviation ((stream category show
                              &key
                              default)
                             &body body)
  "Execute BODY with STREAM bound a stream when SHOW is t or contains
   CATEGORY in the following sense: if CATEGORY is a single category,
   it has to be contained in SHOW, if CATEGORY is a list, it has to
   have a non-empty intersection with SHOW."
  (check-type category (or list keyword) "a keyword or a list of keywords")

  (once-only (stream show)
    `(if (show-help-for? ,category
                         :show    ,show
                         :default ,default)
         (progn ,@body)
         (format ,stream "Use the ~{--help-for=~(~A~) or ~} ~
                          --help-for=all options to display ~
                          the full help text for this item."
                 (ensure-list ,category)))))

;;; URI synopsis for connectors

(defun print-uri-synopsis (connector-class stream)
  "Return a synopsis string for the URI syntax of CONNECTOR-CLASS."
  (let+ (((&accessors-r/o
           (schemas rsb.transport:connector-schemas)
           (options rsb.transport:connector-options)) connector-class)
         (host  (find :host options :key #'first))
         (port  (find :port options :key #'first))
         (other (remove-if (lambda (option)
                             (member (first option) '(:host :port)))
                           options)))
        (format stream
                "~:{~&~(~A~):~:[~;[//HOST]~]~:[~;[:PORT]~]/SCOPE~@[[?~:{~(~A~)=A-~A~^;~}]~]~}"
                (map-product #'cons schemas `((,host ,port ,other))))))

(defun print-all-uri-synopsis (stream)
  "Print synopsis strings for all known transport implementation on
   STREAM."
  (pprint-logical-block (stream nil)
    (iter (for (name class) in (remove-duplicates
                                (rsb.transport:transport-classes)
                                :key  (compose #'rsb.transport:connector-schemas
                                               #'second)
                                :test #'equal))
          (print-uri-synopsis class stream)
          (pprint-newline :mandatory stream))))

;;; URI help

(defun print-uri-help (stream
                       &key
                       (uri-var "URI"))
  "Print an explanatory string regarding the interpretation of URIS
   onto STREAM."
  (format stream
          "SCHEME:[//HOST][:PORT][/PATH][?QUERY][#FRAGMENT]~@
           ~@
           is interpreted as follows:~@
           ~@
           + SCHEME   -> Transport name~@
           + HOST     -> Transport option :HOST (not always supported)~@
           + PORT     -> Transport option :PORT (not always supported)~@
           + PATH     -> Scope~@
           + QUERY    -> \"freestyle\" transport options. Has to be of ~@
                         the form KEY1=VALUE1;KEY2=VALUE2;...~@
           + FRAGMENT -> not processed~@
           ~@
           For the currently available transports, ~A should match the ~
           following patterns:~@
           ~@
           ~2@T"
          uri-var)
  (print-all-uri-synopsis stream))

;;; Filter help

(defun print-filter-help (stream
                          &key
                          (class-blacklist   '(:not :complement
                                               :or :disjoin :and :conjoin
                                               :constant ))
                          (initarg-blacklist '(:intern-scope?)))
  "Format a table of filter names and corresponding documentation
   strings onto STREAM."
  (print-classes-help-string
   (mapcar (lambda (provider)
             (list (service-provider:provider-name provider)
                   (service-provider:provider-class provider)))
           (service-provider:service-providers 'rsb.filter::filter))
   stream
   :class-blacklist   class-blacklist
   :initarg-blacklist initarg-blacklist))

;;; Version string

(defun print-version (version stream
                      &key
                      (program-name          (progname))
                      (include-lisp-version? t)
                      (include-rsb-version?  t)
                      more-versions)
  "Format VERSION onto STREAM as a version number.

   If INCLUDE-LISP-VERSION? is non-nil, additionally format the Lisp
   implementation name and version onto STREAM.

   If INCLUDE-RSB-VERSION? is non-nil, additionally format the RSB
   version onto STREAM.

   MORE-VERSIONS is a \"plist\" of additional component names and
   associated versions that should be printed onto STREAM."
  (let+ (;; Compute the list of all requested versions.
         (versions
          (append `((,program-name ,version))
                  (when include-lisp-version?
                    `((,(lisp-implementation-type)
                        ,(lisp-implementation-version))))
                  (when include-rsb-version?
                    `(("RSB" ,(cl-rsb-system:version/list :commit? t))))
                  (iter (for (name version) on more-versions :by #'cddr)
                        (collect (list (string name) version)))))
         ;; Compute the common column for all versions to be printed
         ;; to.
         (version-column (+ (reduce #'max versions
                                    :key (compose #'length #'first))
                            1
                            (length "version")
                            1))
         ((&flet+ format-version ((name version))
            (format stream "~A version~VT~:[~{~D.~D~^.~D~^-~A~}~;~A~]~&"
                    name version-column (stringp version) version))))
    (map nil #'format-version versions)))

;;; Utility functions

(defun print-classes-help-string (classes stream
                                  &key
                                  class-blacklist
                                  initarg-blacklist)
  "Based on CLASSES, format a table of class names, valid initargs and
   corresponding documentation strings onto STREAM.  BLACKLIST can be
   used to specify classes that should not be processed."
  (let+ ((*print-right-margin* most-positive-fixnum)
         (*print-miser-width*  most-positive-fixnum)
         (items (remove-duplicates
                 (remove-if (rcurry #'member class-blacklist)
                            classes
                            :key #'first)
                 :key #'second))
         ((&flet do-one (name class)
            (let ((args (set-difference (%class-valid-initargs class)
                                        initarg-blacklist))
                  (doc  (%format-documentation
                         (documentation (class-name class) 'type))))
              (list name args doc)))))
    (format stream "~{~{~(~2@T~A~)~<~#[~; [~@{~(~S~) ARG~}]~:; { ~
                    ~@{~(~S~) ARG~^ | ~} }*~]~:>~2&~4@T~@<~@;~A~:>~}~^~%~%~}"
            (map 'list (curry #'apply #'do-one) items))))

(defun %class-valid-initargs (class)
  "Return a list of keywords each of which is an acceptable initarg of
   class."
  (closer-mop:finalize-inheritance class)
  (let+ ((i-i-methods (closer-mop:compute-applicable-methods-using-classes
                       (fdefinition 'initialize-instance)
                       (list class)))
         (s-i-methods (closer-mop:compute-applicable-methods-using-classes
                       (fdefinition 'shared-initialize)
                       (list class (find-class 't))))
         ((&flet keyword-args (method)
            (map 'list #'caar
                 (nth-value 3 (parse-ordinary-lambda-list
                               (closer-mop:method-lambda-list method)))))))
    (remove-duplicates
     (append
      (mappend #'keyword-args i-i-methods)
      (mappend #'keyword-args s-i-methods)
      (remove nil
              (map 'list (compose #'first
                                  #'closer-mop:slot-definition-initargs)
                   (closer-mop:class-slots class)))))))

;;; Utility function

(defun %format-documentation (string)
  "Format STRING as documentation by breaking it into paragraphs and
   removing linebreaks from paragraphs that appear to not have been
   layouted specifically. "
  (let+ (((&flet split-into-paragraphs (string)
            (iter (with rest = string)
                  (let ((index (search #.(format nil "~%~%") rest)))
                    (collect (subseq rest 0 index))
                    (while index)
                    (setf rest (subseq rest (+ index 2)))))))
         ((&flet has-layout? (string)
            (some (rcurry #'search string)
                  `(,(string #\Tab) "  " "* " "+ " "\n- "))))
         ((&flet remove-newlines (string)
            (substitute #\Space #\Newline string))))
    (format nil "~{~A~^~%~%~}"
            (map 'list (lambda (paragraph)
                         (if (has-layout? paragraph)
                             paragraph
                             (remove-newlines paragraph)))
                 (split-into-paragraphs string)))))

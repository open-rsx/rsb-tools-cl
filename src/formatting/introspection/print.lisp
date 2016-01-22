;;;; print.lisp --- Printing of introspection information.
;;;;
;;;; Copyright (C) 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.formatting.introspection)

;;; Utilities

(defun print-elapsed-time (stream start-time &optional colon? at?)
  (let* ((now-time   (local-time:now))
         (difference (local-time:timestamp-difference
                      now-time start-time)))
    (rsb.formatting:print-human-readable-duration
     stream difference colon? at?)))

;;; Participant

(defun print-participant-info-markup (stream participant-info
                                      &optional colon? at?)
  (declare (ignore colon? at?))
  (let+ (((&structure-r/o participant-info- kind id scope type)
          participant-info))
    (format stream "~/rsb::print-id/~
                    ~18,0T~
                    ~:[~
                      ~A~
                      ~35,0T~A<~A>~
                      ~58,0T~A~
                    ~:;~
                      <missing>~
                    ~]"
            id (eq :proxy kind) :active kind type (scope-string scope))))

;;; Process

(defun print-process-state-markup (stream state &optional colon? at?)
  (declare (ignore colon? at?))
  (princ state stream))

(defun print-process-info-markup (stream process-info
                                  &optional colon? at?)
  (declare (ignore colon? at?))
  (let+ ((remote? (typep process-info 'remote-process-info))
         ((&structure
           process-info-
           process-id program-name commandline-arguments display-name state)
          process-info)
         (*print-lines* 1))
    (write-string ; next four lines hack around limitations of nested pretty streams
     (with-output-to-string (stream)
       (let ((*print-right-margin* (when-let ((right-margin *print-right-margin*))
                                     (- right-margin 3))))
         (format stream "~6,,,'0@A~
                         ~17,0T~@[ ~/rsb.formatting.introspection::print-process-state-markup/~]~
                         ~25,0T~@[ (~/rsb.formatting.introspection::print-elapsed-time/)~]~
                         ~35,0T~@<~:[~A~:;~:*~A (~A)~]~@[ ~:_~{~A~^ ~:_~}~]~@:>"
                 process-id
                 (when remote? state)
                 (when remote? (info-most-recent-activity process-info))
                 display-name program-name commandline-arguments)))
     stream)))

(defun print-process-info-details-markup (stream process-info
                                          &optional colon? at?)
  (declare (ignore colon? at?))
  (let+ ((remote? (typep process-info 'remote-process-info))
         ((&accessors (start-time     process-info-start-time)
                      (executing-user process-info-executing-user)
                      (rsb-version    process-info-rsb-version)
                      (transports     process-info-transports)
                      (latency        info-latency))
          process-info))
   (format stream "Uptime    ~@[ ~/rsb.formatting.introspection::print-elapsed-time/~]~
                   ~24,0T│ User        ~:[?~:;~:*~A~]~
                   ~@:_Latency ~/rsb.formatting.introspection::print-time-offset-markup/~
                   ~24,0T│ RSB Version ~:[?~:;~:*~A~]~
                   ~@:_Transports~@[ ~@<~{~A~^, ~_~}~:>~]"
           start-time             executing-user
           (when remote? latency) rsb-version
           (when remote?
             (mapcar (lambda (uri) (puri:merge-uris (puri:uri "/") uri))
                     transports)))))

;;; Host

(defun print-host-state-markup (stream state &optional colon? at?)
  (declare (ignore colon? at?))
  (princ state stream))

(defun print-time-offset-markup (stream value &optional colon? at?)
  (declare (ignore colon? at?))
  (format stream "~[~
                     ??.??? s~
                     ~;~
                     ~,3@F s~
                     ~;~
                     ~2@T< 1 ms~
                  ~]"
          (cond
            ((null value)          0)
            ((> (abs value) 0.001) 1)
            (t                     2))
          value))

(defun print-host-info-markup (stream host-info &optional colon? at?)
  (declare (ignore colon? at?))
  (let* ((remote?              (typep host-info 'remote-host-info))
         (state                (when remote? (host-info-state host-info)))
         (most-recent-activity (when remote? (info-most-recent-activity host-info))))
    (format stream "~A~
                    ~17,0T~@[ ~/rsb.formatting.introspection::print-host-state-markup/~]~
                    ~25,0T~@[ (~/rsb.formatting.introspection::print-elapsed-time/)~]"
            (host-info-hostname host-info) state most-recent-activity)))

(defun truncate-string (string max)
  (let ((length (length string)))
    (if (> length max)
        (concatenate 'string "…" (subseq string (- length max -1) length))
        string)))

(defun print-host-info-details-markup (stream host-info &optional colon? at?)
  (declare (ignore colon? at?))
  (let+ (((&structure-r/o
           host-info-
           machine-type machine-version software-type software-version)
          host-info)
         (remote?      (typep host-info 'remote-host-info))
         (clock-offset (when remote? (info-clock-offset host-info)))
         (latency      (when remote? (info-latency host-info))))
    (format stream "Clock offset ~/rsb.formatting.introspection::print-time-offset-markup/~
                    ~24,0T│ Machine type    ~:[?~:;~:*~A~]~
                    ~60,0T│ Software type    ~:[?~:;~:*~A~]~
                    ~@:_Latency      ~/rsb.formatting.introspection::print-time-offset-markup/~
                    ~24,0T│ Machine version ~:[?~:;~:*~A~]~
                    ~60,0T│ Software version ~:[?~:;~:*~A~]"
     clock-offset machine-type                         software-type
     latency      (truncate-string machine-version 17) software-version)))

;;; Tree printing

(defgeneric entry-children-for-printing (entry)
  (:method ((entry t))
    (node-children entry))
  (:method ((entry participant-entry))
    (sort-participants (node-children entry)))
  (:method ((entry process-entry))
    (sort-participants (node-children entry))))

(defgeneric print-entry (target entry what &key filter))

(defmethod print-entry ((target stream)
                        (entry  rsb.model:info-mixin)
                        (what   t)
                        &key &allow-other-keys)
  (print-entry target (node-info entry) what))

(defmethod print-entry ((target stream)
                        (entry  participant-info)
                        (what   (eql :first))
                         &key &allow-other-keys)
  (print-participant-info-markup target entry)
  nil) ; return value indicates "no further content"

(defmethod print-entry ((target stream)
                        (entry  process-info)
                        (what   (eql :first))
                         &key &allow-other-keys)
  (print-process-info-markup target entry)
  t) ; return value indicates "more content available"

(defmethod print-entry ((target stream)
                        (entry  process-info)
                        (what   (eql :content))
                         &key &allow-other-keys)
  (print-process-info-details-markup target entry))

(defmethod print-entry ((target stream)
                        (entry  host-info)
                        (what   (eql :first))
                         &key &allow-other-keys)
  (print-host-info-markup target entry)
  t) ; return value indicates "more content available"

(defmethod print-entry ((target stream)
                        (entry  host-info)
                        (what   (eql :content))
                         &key &allow-other-keys)
  (print-host-info-details-markup target entry))

(defmethod print-entry ((target stream)
                        (entry  remote-introspection-database)
                        (what   t)
                        &rest args &key filter &allow-other-keys)
  (declare (ignore filter))
  (apply #'print-object-tree1 entry target args))

(defmethod print-entry ((target stream)
                        (entry  remote-introspection)
                        (what   t)
                        &rest args &key &allow-other-keys)
  (with-database-lock (entry) ; TODO let client lock
    (apply #'print-entry target (introspection-database entry) :first args)))

;;; Entry utility functions

(defun print-object-tree1 (tree stream &rest args
                           &key
                           (filter (constantly t)))
  (let* ((args    (remove-from-plist args :filter))
         (printer (utilities.print-tree:make-folding-node-printer
                   (lambda (stream depth entry)
                     (declare (ignore depth))
                     (apply #'print-entry stream entry :first args))
                   (lambda (stream depth entry)
                     (declare (ignore depth))
                     (apply #'print-entry stream entry :content args))
                   #'entry-children-for-printing
                   (lambda (depth node)
                     (funcall filter stream node t :depth depth))))
         (roots   (entry-children-for-printing tree)))
    (pprint-logical-block (stream roots)
      (if roots
          (mapc (lambda (root)
                  (utilities.print-tree:print-tree stream root printer)
                  (pprint-newline :mandatory stream))
                roots)
          (format stream "<no entries>~@:_")))))

(defun sort-participants (participants)
  (sort (copy-list participants) #'string<
        :key (compose #'scope-string
                      #'participant-info-scope
                      #'node-info)))

;; Local Variables:
;; coding: utf-8
;; End:
;;;; styles.lisp --- Printing and other processing of introspection information.
;;;;
;;;; Copyright (C) 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

;; TODO (cl:in-package #:rsb.formatting.introspection)
(cl:in-package #:rsb.tools.introspect)

(defclass style-graph (database-mixin)
  ((delay :initarg  :delay
          :type     (or null positive-real)
          :reader   style-delay
          :documentation
          "Amount of time in seconds to wait before printing a
           snapshot of the introspection database."))
  (:default-initargs
   :delay 1.0)
  (:documentation
   "Quickly gather introspection information and draw a graph.

    The default behavior is gathering information for one second then
    drawing a graph of the resulting snapshot. In most systems, one
    second should be enough to request and obtain all available
    introspection information. However, circumstances like degraded
    system performances or extreme communication latency may required
    longer delays."))

(service-provider:register-provider/class
 'style :graph :class 'style-graph)

(defmethod detach ((participant style-graph)))

;; Introspection event => ignore.
(defmethod rsb.ep:handle ((sink style-graph) (data list)))

;; Dummy event => print the object tree snapshot and return false to
;; indicate that the program should be terminated.
(defmethod rsb.formatting:format-event ((event  t)
                                        (style  style-graph)
                                        (target t)
                                        &key &allow-other-keys)

  (let+ (((&structure-r/o style- database delay) style))
    (sleep delay)
    (let* ((database1 (rsb.introspection::introspection-database database))
           (graph (rsb.introspection:with-database-lock (database)
                    (cl-dot:generate-graph-from-roots
                     (make-instance 'introspection-graph
                                    :parents      (find-parents database1)
                                    :participants (introspection-participants/roots database1))
                     (list database)))))
      #+no (cl-dot:print-graph graph)
      (cl-dot:dot-graph graph "/tmp/bla.svg" :format :svg)))
  nil)

;;; Participant "connections"

(defmethod participants-communicate? ((from participant-entry)
                                      (to   participant-entry))
  (or (participants-communicate? (entry-info from) (entry-info to))
      (when-let ((children (entry-children from)))
        (some (rcurry #'participants-communicate? to) children))
      (when-let ((children (entry-children to)))
        (some (curry #'participants-communicate? from) children))))

(defmethod participants-communicate?
    ((from rsb.introspection:participant-info)
     (to   rsb.introspection:participant-info))
  (participants-communicate-using-kinds?
   from to
   (rsb.introspection:participant-info-kind from)
   (rsb.introspection:participant-info-kind to)))

(defmethod participants-communicate-using-kinds?
    ((from      rsb.introspection:participant-info)
     (to        rsb.introspection:participant-info)
     (from-kind t)
     (to-kind   t))
  nil)

(defmethod participants-communicate-using-kinds?
    ((from      rsb.introspection:participant-info)
     (to        rsb.introspection:participant-info)
     (from-kind (eql :informer))
     (to-kind   (eql :listener)))
  (sub-scope? (rsb.introspection:participant-info-scope from)
              (rsb.introspection:participant-info-scope to)))

(defmethod participants-communicate-using-kinds?
    ((from      rsb.introspection:participant-info)
     (to        rsb.introspection:participant-info)
     (from-kind (eql :local-method))
     (to-kind   (eql :remote-method)))
  (scope= (rsb.introspection:participant-info-scope from)
          (rsb.introspection:participant-info-scope to)))

(defmethod participants-communicate-using-kinds?
    ((from      rsb.introspection:participant-info)
     (to        rsb.introspection:participant-info)
     (from-kind (eql :remote-method))
     (to-kind   (eql :local-method)))
  (scope= (rsb.introspection:participant-info-scope from)
          (rsb.introspection:participant-info-scope to)))
;;; Graph construction

(defun find-parents (database)
  (let+ ((table (make-hash-table :test #'eq))
         ((&labels do-entry (entry)
            (dolist (child (entry-children entry))
              (setf (gethash child table) entry)
              (do-entry child)))))
    (do-entry database)
    table))

(defclass introspection-graph ()
  ((parents     :initarg  :parents ; TODO hack?
                :reader   graph-parents)
   (particpants :initarg  :participants
                :type     list
                :reader   graph-participants)))

(defmethod cl-dot::graph-object-in-cluster ((graph introspection-graph) (object t))
  nil)

(defmethod cl-dot:graph-object-node ((graph introspection-graph) (object t))
  nil)

;; `remote-introspect'

(defmethod cl-dot:graph-object-knows-of
    ((graph  introspection-graph)
     (object rsb.introspection::remote-introspection))
  (list (rsb.introspection::introspection-database object)))

;; `remote-introspection-database'

(defmethod cl-dot:graph-object-knows-of
    ((graph  introspection-graph)
     (object rsb.introspection::remote-introspection-database))
  (introspection-hosts object))

;; `host-entry'

(defmethod cl-dot::graph-object-cluster ((graph  introspection-graph)
                                         (object host-entry))
  (let+ (((&flet+ make-row ((label . value))
            `(:tr () (:td ((:align "left")) ,label) (:td ,@(if (typep value 'cons)
                                                               value
                                                               (list () (format nil "~A" value)))))))
         (info (rsb.introspection:entry-info object))
         ((&structure-r/o rsb.introspection::host-info- id hostname state)
          info)
         (rows `(("state" . (() (:font ((:color "red")) ,(string-downcase state))))))
         (label `(:html ()
                  (:table ((:border "0"))
                          (:tr () (:td ((:colspan "2")) (:table ((:border "0")) (:tr () (:td () (:img ((:src "/tmp/gnome-dev-computer.svg")))) (:td () ,hostname)))))
                   ,@(mapcar #'make-row rows)))))
    (make-instance 'cl-dot::cluster
                   :id         id
                   :attributes (list :label label))))

(defmethod cl-dot:graph-object-knows-of ((graph  introspection-graph)
                                         (object host-entry))
  (introspection-processes object))

;; `process-entry'

(defmethod cl-dot::graph-object-cluster ((graph  introspection-graph)
                                         (object process-entry))
  (let+ (((&flet+ make-row ((label . value))
            `(:tr () (:td ((:align "left")) ,label) (:td ,@(if (typep value 'cons)
                                                               value
                                                               (list () (format nil "~A" value)))))))
         (info (rsb.introspection:entry-info object))
         ((&structure-r/o rsb.introspection::process-info- state process-id program-name display-name executing-user start-time) ; TODO sort
          info)
         (rows `(("pid"    . ,process-id)
                 ("state"  . (() (:font ((:color "red")) ,(string-downcase state))))
                 ("user"   . ,executing-user)
                 ("uptime" . ,(with-output-to-string (stream)
                                (print-elapsed-time stream start-time t)))))
         (label `(:html ()
                  (:table ((:border "0"))
                   (:tr () (:td ((:colspan "2")) (:table ((:border "0")) (:tr () (:td () (:img ((:src "/tmp/applications-other.svg")))) (:td () ,(or program-name display-name))))))
                   ,@(mapcar #'make-row rows)))))
    (make-instance 'cl-dot::cluster
                   :id process-id
                  :attributes (list :label label
                       ))))

(defmethod cl-dot:graph-object-knows-of ((graph  introspection-graph)
                                         (object process-entry))
  (introspection-participants/roots object))

(defmethod cl-dot::graph-object-in-cluster ((graph  introspection-graph)
                                            (object process-entry))
  (gethash object (graph-parents graph)))

;; `participant-entry'

(defmethod cl-dot:graph-object-node ((graph  introspection-graph)
                                     (object participant-entry))
  (let+ (((&flet+ make-row ((label . value))
            `(:tr () (:td ((:align "left")) ,label) (:td ,@(if (typep value 'cons)
                                                               value
                                                               (list () (format nil "~A" value)))))))
         (info (rsb.introspection:entry-info object))
         ((&structure-r/o rsb.introspection::participant-info- id kind scope) ; TODO sort
          info)
         (rows `(("scope" . (((:align "left")) ,(scope-string scope)))))
         (label `(:html ()
                        (:table ((:border "0"))
                                (:tr () (:td ((:colspan "2"))
                                             (:table ((:border "0"))
                                                     (:tr ()
                                                          (:td () (:img ((:src "/tmp/network-wireless.svg"))))
                                                          (:td () ,(format nil "~A ~/rsb::print-id/"
                                                                          (string-downcase kind)
                                                                          id))))))
                                ,@(mapcar #'make-row rows)))))
    (make-instance 'cl-dot:node
                   :attributes (list :label label
                                     :shape :box))))

(defmethod cl-dot:graph-object-points-to ((graph  introspection-graph)
                                          (object participant-entry))
  (remove-if-not (curry #'participants-communicate? object)
                 (remove object (graph-participants graph))))

(defmethod cl-dot::graph-object-in-cluster ((graph  introspection-graph)
                                            (object participant-entry))
  (gethash object (graph-parents graph)))

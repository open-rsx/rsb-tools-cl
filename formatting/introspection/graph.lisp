;;;; graph.lisp --- TODO.
;;;;
;;;; Copyright (C) 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.formatting.introspection)

(define-constant +default-graph-attributes+
    '(:fontname   "Arial"
      :fontsize   11
      :rankdir    "LR"
      :stylesheet "foo.css"
      :node       (:fontsize  11
                   :fontname  "Arial"
                   :style     :filled)
      :edge       (:fontsize  11
                   :fontname  "Arial"))
  :test #'equal)

(defstruct (graph-parameters
             (:copier nil))
  (levels           nil                        :type list)
  (node-filter      nil                        :type (or symbol function))
  (edge-filter      nil                        :type (or symbol function))
  (graph-attributes +default-graph-attributes+ :type list))

(defclass style-graph (database-mixin)
  ((delay            :initarg  :delay
                     :type     (or null positive-real)
                     :reader   style-delay
                     :documentation
                     "Amount of time in seconds to wait before
                      printing a snapshot of the introspection
                      database.")
   (graph-parameters :accessor style-%graph-parameters
                     :initform (make-graph-parameters)))
  (:default-initargs
   :delay       1.0
   :levels      '(1 2) ; TODO (1 3) does not currently work
   :node-filter (constantly nil)
   :edge-filter (either-end #'listener-on-root-scope?))
  (:documentation
   "Quickly gather introspection information and draw a graph.

    The default behavior is gathering information for one second then
    drawing a graph of the resulting snapshot. In most systems, one
    second should be enough to request and obtain all available
    introspection information. However, circumstances like degraded
    system performances or extreme communication latency may required
    longer delays."))

(defmethod shared-initialize :after
    ((instance   style-graph)
     (slot-names t)
     &key
     (levels      nil levels-supplied?)
     (node-filter nil node-filter-supplied?)
     (edge-filter nil edge-filter-supplied?))
  (let ((parameters (style-%graph-parameters instance)))
    (when levels-supplied?
      (setf (graph-parameters-levels parameters) levels))
    (when node-filter-supplied?
      (setf (graph-parameters-node-filter parameters) node-filter))
    (when edge-filter-supplied?
      (setf (graph-parameters-edge-filter parameters) edge-filter))))

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
  (let+ (((&structure-r/o style- database delay (parameters %graph-parameters)) style))
    ;; Wait for introspection survey responses to arrive.
    (when delay (sleep delay))
    (let ((graph (with-database-lock (database)
                   (generate-graph
                    (rsb.introspection::introspection-database database)
                    parameters))))
      #+no (cl-dot:print-graph graph)
      (let* ((process
              (uiop/run-program::%run-program
               (list "dot" "-Tsvg")
               :input :stream :error-output *error-output* :output target
               :wait nil))
             (input-stream (getf process :input-stream)))
        (cl-dot:print-graph graph :stream input-stream)
        (close input-stream)
        (unless (zerop (uiop/run-program::%wait-process-result process))
          (error "TODO")))
      #+no (cl-dot:dot-graph graph "/tmp/bla.svg" :format :svg)))
  nil)

;;; Graph construction

(defclass introspection-graph ()
  ((parents     :initarg  :parents ; TODO hack
                :reader   graph-parents)
   (processes   :initarg  :processes
                :type     list
                :reader   graph-processes)
   (particpants :initarg  :participants
                :type     list
                :reader   graph-participants)
   (parameters  :initarg  :parameters
                :reader   graph-parameters)))

(defun generate-graph (database parameters)
  (let+ ((parents (make-hash-table :test #'eq)) ; TODO remove
         ((&labels do-entry (entry)
            (dolist (child (entry-children entry))
              (setf (gethash child parents) entry)
              (do-entry child)))))
    ;; Build parents table.
    (do-entry database)
    ;; Build graph.
    (cl-dot:generate-graph-from-roots
     (make-instance 'introspection-graph
                    :parents      parents
                    :processes    (introspection-processes database)
                    :participants (introspection-participants/roots database)
                    :parameters   parameters)
     (list database)
     (list*
      #+no :label #+no "RSB Components TODO"
      (graph-parameters-graph-attributes parameters)))))

;; generic filters

(defmethod cl-dot:graph-object-node :around ((graph  introspection-graph)
                                             (object t))
  (unless (funcall (graph-parameters-node-filter (graph-parameters graph)) object)
    (call-next-method)))

(defmethod cl-dot:graph-object-points-to :around ((graph  introspection-graph)
                                                  (object t))
  (remove-if (curry (graph-parameters-edge-filter (graph-parameters graph)) object)
             (call-next-method)))

(macrolet ((define-level-restriction-methods (class level)
             `(defmethod cl-dot:graph-object-node :around ((graph  introspection-graph)
                                                           (object ,class))
                (when (member ,level (graph-parameters-levels (graph-parameters graph)))
                  (call-next-method)))))
  (define-level-restriction-methods host-entry        1)
  (define-level-restriction-methods process-entry     2)
  (define-level-restriction-methods participant-entry 3))

;; `remote-introspection-database'

(defmethod cl-dot:graph-object-node
    ((graph  introspection-graph)
     (object rsb.introspection::remote-introspection-database))
  nil)

(defmethod cl-dot:graph-object-knows-of
    ((graph  introspection-graph)
     (object rsb.introspection::remote-introspection-database))
  (introspection-hosts object))

;; `host-entry'

(defmethod cl-dot:graph-object-node ((graph  introspection-graph)
                                     (object host-entry))
  (let+ (((&structure-r/o rsb.introspection::host-info- id hostname state)
          (entry-info object))
         (label (make-label-table
                 "gnome-dev-computer.svg"
                 hostname (string-downcase state))))
    (make-instance 'cl-dot:cluster
                   :id         id
                   :attributes (list :label label))))

(defmethod cl-dot:graph-object-knows-of ((graph  introspection-graph)
                                         (object host-entry))
  (introspection-processes object))

;; `process-entry'

(defmethod cl-dot:graph-object-node ((graph  introspection-graph)
                                     (object process-entry))
  (let+ (((&structure-r/o
           rsb.introspection::process-info- state process-id program-name display-name executing-user start-time) ; TODO sort
          (entry-info object))
         (label (make-label-table
                 "applications-other.svg"
                 (format nil "~A [~A] ~A"
                         (or display-name program-name) process-id executing-user)
                 (format nil "~(~A~) (~:/rsb.formatting.introspection::print-elapsed-time/)"
                         state start-time))))
    (make-instance 'cl-dot:cluster
                   :id         process-id ; TODO add host id; not unique otherwise
                   :attributes (list :label label :tooltip program-name)))) ; TODO tooltips in general

(defmethod cl-dot:graph-object-knows-of ((graph  introspection-graph)
                                         (object process-entry))
  (introspection-participants/roots object))

(defmethod cl-dot:graph-object-points-to ((graph  introspection-graph)
                                          (object process-entry))
  (unless (member 3 (graph-parameters-levels (graph-parameters graph))) ; TODO generic filter?
    (remove-if (complement (curry #'entities-communicate? object))
               (remove object (graph-processes graph)))))

(defmethod cl-dot:graph-object-contained-by ((graph  introspection-graph) ; TODO remove; use contains for host instead
                                             (object process-entry))
  (gethash object (graph-parents graph)))

;; `participant-entry'

(defmethod cl-dot:graph-object-node ((graph  introspection-graph)
                                     (object participant-entry))
  (let+ ((info (entry-info object))
         ((&structure-r/o rsb.introspection::participant-info- id kind scope) ; TODO sort
          info)
         (label (make-label-table
                 "network-wireless.svg"
                 (format nil "~A [~/rsb::print-id/]"
                         (string-downcase kind) id)
                 (scope-string scope))))
    (make-instance 'cl-dot:node
                   :attributes (list* :label label
                                      :shape :box
                                      (when (listener-on-root-scope? info)
                                        (list :style     :filled
                                              :fillcolor "#ff9090"))))))

(defmethod cl-dot:graph-object-points-to ((graph  introspection-graph)
                                          (object participant-entry))
  (remove-if (complement (curry #'entities-communicate? object))
             (remove object (graph-participants graph))))

(defmethod cl-dot::graph-object-contained-by ((graph  introspection-graph) ; TODO remove; use contains for process instead
                                              (object participant-entry))
  (gethash object (graph-parents graph)))

;;; Utilities

(defun make-label-table (icon first-line second-line)
  `(:html ()
    (:table ((:border "0") (:cellspacing "0") (:cellpadding "0"))
     (:tr () (:td ((:rowspan "2"))    (:img ((:src ,icon))))
             (:td ((:align   "left")) ,first-line))
     (:tr () (:td ((:align   "left")) ,second-line)))))

(defun either-end (predicate)
  (lambda (from to)
    (or (funcall predicate from) (funcall predicate to))))

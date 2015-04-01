;;;; styles.lisp --- Printing and other processing of introspection information.
;;;;
;;;; Copyright (C) 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

;; TODO (cl:in-package #:rsb.formatting.introspection)
(cl:in-package #:rsb.tools.introspect)

(defstruct (graph-parameters
             (:copier nil))
  (levels      nil :type list)
  (node-filter nil :type (or symbol function))
  (edge-filter nil :type (or symbol function)))

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
     (levels nil levels-supplied?)
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

;;; Participant "connections" TODO new package like rsb.meta.inference?

(defgeneric entities-communicate? (from to)
  (:documentation
   "TODO"))

(defgeneric participants-communicate-using-kinds? (form to from-kind to-kind)
  (:documentation
   "TODO"))

(macrolet ((define-entities-communicate?-methods (class accessor)
             `(progn
                (defmethod entities-communicate? ((from ,class)
                                                  (to   t))
                  (when-let ((processes (,accessor from)))
                    (some (rcurry #'entities-communicate? to) processes)))

                (defmethod entities-communicate? ((from t)
                                                  (to   ,class))
                  (when-let ((processes (,accessor to)))
                    (some (curry #'entities-communicate? from) processes))))))
  (define-entities-communicate?-methods host-entry
      introspection-processes)
  (define-entities-communicate?-methods process-entry
      introspection-participants/roots))

(defmethod entities-communicate? ((from participant-entry)
                                  (to   participant-entry))
  (or (entities-communicate? (entry-info from) (entry-info to))
      (when-let ((children (entry-children from)))
        (some (rcurry #'entities-communicate? to) children))
      (when-let ((children (entry-children to)))
        (some (curry #'entities-communicate? from) children))))

(defmethod entities-communicate? ((from participant-info)
                                  (to   participant-info))
  (participants-communicate-using-kinds?
   from to (participant-info-kind from) (participant-info-kind to)))

(defmethod participants-communicate-using-kinds?
    ((from      participant-info)
     (to        participant-info)
     (from-kind t)
     (to-kind   t))
  nil)

(defmethod participants-communicate-using-kinds?
    ((from      participant-info)
     (to        participant-info)
     (from-kind (eql :informer))
     (to-kind   (eql :listener)))
  (sub-scope? (participant-info-scope from) (participant-info-scope to)))

(defmethod participants-communicate-using-kinds?
    ((from      participant-info)
     (to        participant-info)
     (from-kind (eql :local-method))
     (to-kind   (eql :remote-method)))
  (scope= (participant-info-scope from) (participant-info-scope to)))

(defmethod participants-communicate-using-kinds?
    ((from      participant-info)
     (to        participant-info)
     (from-kind (eql :remote-method))
     (to-kind   (eql :local-method)))
  (scope= (participant-info-scope from) (participant-info-scope to)))

;;; Graph construction

(defclass introspection-graph ()
  ((parents     :initarg  :parents ; TODO hack?
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
  (let+ ((parents (make-hash-table :test #'eq))
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
     '(:fontname "Arial"
       :fontsize 11
       :rankdir "LR"
       ;; :stylesheet "../_static/corlab.css"
       :node (:fontsize 11)
       :node (:fontname "Arial")
       :edge (:fontsize 11)
       :edge (:fontname "Arial")))))

;; TODO move to cl-dot?
(defmethod cl-dot::graph-object-in-cluster ((graph introspection-graph) (object t))
  nil)

(defmethod cl-dot:graph-object-node ((graph introspection-graph) (object t))
  nil)

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
             `(progn
                (defmethod cl-dot:graph-object-node :around ((graph  introspection-graph)
                                                             (object ,class))
                  (when (member ,level (graph-parameters-levels (graph-parameters graph)))
                    (call-next-method)))

                (defmethod cl-dot::graph-object-cluster :around ((graph  introspection-graph)
                                                                 (object ,class))
                  (when (member ,level (graph-parameters-levels (graph-parameters graph)))
                    (call-next-method))))))
  (define-level-restriction-methods host-entry        1)
  (define-level-restriction-methods process-entry     2)
  (define-level-restriction-methods participant-entry 3))

;; `remote-introspection-database'

(defmethod cl-dot:graph-object-knows-of
    ((graph  introspection-graph)
     (object rsb.introspection::remote-introspection-database))
  (introspection-hosts object))

;; `host-entry'

;; TODO repeated for process below
(flet ((cluster-or-node (object class
                         &rest attributes &key &allow-other-keys)
         (let+ (((&structure-r/o rsb.introspection::host-info- id hostname state)
                 (entry-info object))
                (label (make-label-table
                        "/tmp/gnome-dev-computer.svg"
                        hostname (string-downcase state))))
           (make-instance class
                          :id         id
                          :attributes (list* :label label attributes)))))

  (defmethod cl-dot::graph-object-node ((graph  introspection-graph)
                                        (object host-entry))
    (unless (some (lambda (p) ; TODO simplify
                    (or (cl-dot:graph-object-node graph p)
                        (cl-dot::graph-object-cluster graph p)))
                  (cl-dot:graph-object-knows-of graph object))
      (cluster-or-node object 'cl-dot:node :shape :box)))

  (defmethod cl-dot::graph-object-cluster ((graph  introspection-graph)
                                           (object host-entry))
    (when (some (lambda (p)
                  (or (cl-dot:graph-object-node graph p)
                      (cl-dot::graph-object-cluster graph p)))
                (cl-dot:graph-object-knows-of graph object))
      (cluster-or-node object 'cl-dot::cluster))))

(defmethod cl-dot:graph-object-knows-of ((graph  introspection-graph)
                                         (object host-entry))
  (introspection-processes object))

;; `process-entry'

(flet ((cluster-or-node (object class
                         &rest attributes &key &allow-other-keys)
         (let+ (((&structure-r/o
                  rsb.introspection::process-info- state process-id program-name display-name executing-user start-time) ; TODO sort
                 (entry-info object))
                (label (make-label-table
                        "/tmp/applications-other.svg"
                        (format nil "~A [~A] ~A"
                                (or display-name program-name) process-id executing-user)
                        (format nil "~(~A~) (~:/rsb.tools.introspect::print-elapsed-time/)"
                                state start-time))))
           (make-instance class
                          :id         process-id ; TODO add host id
                          :attributes (list* :label label attributes)))))

  (defmethod cl-dot:graph-object-node ((graph  introspection-graph)
                                       (object process-entry))
    (unless (some (curry #'cl-dot:graph-object-node graph)
                  (cl-dot:graph-object-knows-of graph object))
      (cluster-or-node object 'cl-dot::node :shape :box)))

  (defmethod cl-dot::graph-object-cluster ((graph  introspection-graph)
                                           (object process-entry))
    (when (some (curry #'cl-dot:graph-object-node graph)
                (cl-dot:graph-object-knows-of graph object))
      (cluster-or-node object 'cl-dot::cluster))))

(defmethod cl-dot:graph-object-knows-of ((graph  introspection-graph)
                                         (object process-entry))
  (introspection-participants/roots object))

(defmethod cl-dot:graph-object-points-to ((graph  introspection-graph)
                                          (object process-entry))
  (unless (member 3 (graph-parameters-levels (graph-parameters graph))) ; TODO generic filter?
    (remove-if (complement (curry #'entities-communicate? object))
               (remove object (graph-processes graph)))))

(defmethod cl-dot::graph-object-in-cluster ((graph  introspection-graph)
                                            (object process-entry))
  (gethash object (graph-parents graph)))

;; `participant-entry'

(defmethod cl-dot:graph-object-node ((graph  introspection-graph)
                                     (object participant-entry))
  (let+ ((info (entry-info object))
         ((&structure-r/o rsb.introspection::participant-info- id kind scope) ; TODO sort
          info)
         (label (make-label-table
                 "/tmp/network-wireless.svg"
                 (format nil "~A [~/rsb::print-id/]"
                         (string-downcase kind) id)
                 (scope-string scope))))
    (make-instance 'cl-dot:node
                   :attributes (list* :label label
                                      :shape :box
                                      (when (listener-on-root-scope? info)
                                        (list :style   :filled
                                              :fillcolor "#ff9090"))))))

(defmethod cl-dot:graph-object-points-to ((graph  introspection-graph)
                                          (object participant-entry))
  (remove-if (complement (curry #'entities-communicate? object))
             (remove object (graph-participants graph))))

(defmethod cl-dot::graph-object-in-cluster ((graph  introspection-graph)
                                            (object participant-entry))
  (gethash object (graph-parents graph)))

;;; Utilities

(defun make-label-table (icon first-line second-line)
  `(:html ()
    (:table ((:border "0") (:cellspacing "0") (:cellpadding "0"))
     (:tr () (:td ((:rowspan "2"))    (:img ((:src ,icon))))
             (:td ((:align   "left")) ,first-line))
     (:tr () (:td ((:align   "left")) ,second-line)))))

(defun listener-on-root-scope? (thing)
  (typecase thing
    (process-entry
     (every #'listener-on-root-scope?
            (introspection-participants/roots thing)))
    (participant-entry
     (listener-on-root-scope? (entry-info thing)))
    (participant-info
     (and (eq (participant-info-kind thing) :listener)
          (scope= (participant-info-scope thing) "/")))))

(defun either-end (predicate)
  (lambda (from to)
    (or (funcall predicate from) (funcall predicate to))))

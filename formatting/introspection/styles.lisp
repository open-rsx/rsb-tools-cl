;;;; styles.lisp --- Printing and other processing of introspection information.
;;;;
;;;; Copyright (C) 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.formatting.introspection)

;;; Service

(service-provider:define-service style
  (:documentation
   "Providers of this service implement printing and other processing
    of introspection information"))

;;; `database-mixin'

(defclass database-mixin ()
  ((database :initarg  :database
             :accessor style-database))
  (:documentation
   "This class is intended to be mixed into style classes that have to
    know the introspection database object."))

;;; `events-mixin'

(defclass events-mixin ()
  ((connections :accessor style-connections
                :initform '()))
  (:documentation
   "This class is intended to be mixed into style classes that process
    introspection database events.

    It relieves style classes from the burden of managing handlers for
    hooks of individual introspection database objects as these are
    created and destroyed."))

(defmethod detach ((participant events-mixin))
  ;; Remove all remaining handlers from the respective hooks.
  (iter (for (subject handler) in (style-connections participant))
        (hooks:remove-from-hook (database-change-hook subject) handler)))

(defmethod rsb.ep:handle ((sink events-mixin) (data list))
  ;; Register and unregister handlers as database objects are added
  ;; and removed. Delegate actual processing of events to methods on
  ;; `format-event'.
  (let+ (((&structure style- connections) sink)
         ((&flet register (subject function)
            (let* ((hook       (database-change-hook subject))
                   (connection (list subject (curry function subject))))
              (push connection connections)
              (hooks:add-to-hook hook (second connection)))))
         ((&flet unregister (subject)
            (when-let* ((hook       (database-change-hook subject))
                        (connection (find subject connections :key #'first)))
              (hooks:remove-from-hook hook (second connection))
              (removef connections connection))))
         ((&labels process-event (object event subject)
            (rsb.formatting:format-event
             (list (local-time:now) object event subject) sink t)))
         ((&labels on-process-change (object subject event)
            (process-event object event subject)))
         ((&labels on-host-change (object subject event)
            (process-event object event subject)
            (case event
              (:process-added   (register subject #'on-process-change))
              (:process-removed (unregister subject)))))
         ((&labels on-database-change (object subject event)
            (process-event object event subject)
            (case event
              (:host-added   (register subject #'on-host-change))
              (:host-removed (unregister subject))))))
    (apply #'on-database-change :introspection data)))

;;; `event-buffer-mixin'

(defclass event-buffer-mixin ()
  ((events :type     list
           :accessor style-events
           :initform '()
           :documentation
           "Buffers introspection events for periodic
            batch-processing."))
  (:documentation
   "This class is intended to be mixed into style classes that
    accumulate asynchronously received introspection database events
    and periodically process batches of these events."))

(defmethod rsb.formatting:format-event ((event  list)
                                        (style  events-mixin)
                                        (target t)
                                        &key &allow-other-keys)
  (push event (style-events style)))

;;; `object-tree-printing-mixin'

(defclass object-tree-printing-mixin ()
  ((max-depth :initarg  :max-depth
              :type     (or null positive-integer)
              :reader   style-max-depth
              :initform nil
              :documentation
              "When not null, a positive integer indicating the
               maximum depth up to which the tree of introspection
               objects should be printed.

               Nodes below that depth should not be printed."))
  (:documentation
   "This class is intended to be mixed into style classes which print
    trees of introspection objects."))

(defun print-object-tree (database stream max-depth)
  (let+ (((&flet filter (target entry what &key depth)
            (declare (ignore target entry what))
            (cond
              ((or (not max-depth) (not depth) (< depth max-depth))
               '(:first :content :children))
              ((or (not max-depth) (not depth) (<= depth max-depth))
               '(:first :content))))))
    (print-entry stream database t :filter #'filter)))

;;; `style-monitor/events'

(defclass style-monitor/events (database-mixin
                                events-mixin)
  ()
  (:documentation
   "Print relevant introspection events on the output stream.

    Does not print clock-offset- and latency-change events."))

(service-provider:register-provider/class
 'style :monitor/events :class 'style-monitor/events)

;; Dummy event => return true to indicate that the program should
;; continue.
(defmethod rsb.formatting:format-event ((event  t)
                                        (style  style-monitor/events)
                                        (target t)
                                        &key &allow-other-keys)
  t)

(defmethod rsb.formatting:format-event ((event  list)
                                        (style  style-monitor/events)
                                        (target t)
                                        &key &allow-other-keys)
  (let+ (((timestamp object event subject) event)
         ((&flet print-items (thing)
            (typecase thing
              (symbol          `((:name ,(string-downcase thing))))
              (standard-object (print-items:print-items thing))
              (t               `((:value ,(princ-to-string thing))))))))
    (unless (member event '(:clock-offset-changed :latency-changed))
      (format target "~A ~
                      ~32@<~/print-items:format-print-items/~> ~
                      ~20@<~/print-items:format-print-items/~> ~
                      ~/print-items:format-print-items/~
                      ~%"
              timestamp
              (print-items object)
              (print-items event)
              (print-items subject)))))

;;; `style-monitor/object-tree'

(defclass style-monitor/object-tree (database-mixin
                                     object-tree-printing-mixin
                                     rsb.formatting:periodic-printing-mixin
                                     rsb.formatting:separator-mixin)
  ()
  (:default-initargs
   :separator :clear)
  (:documentation
   "Periodically print a snapshot of the introspection tree."))

(service-provider:register-provider/class
 'style :monitor/object-tree :class 'style-monitor/object-tree)

(defmethod detach ((participant style-monitor/object-tree)))

;; Introspection event => ignore.
(defmethod rsb.ep:handle ((sink style-monitor/object-tree) (data list)))

;; Dummy event => return true to indicate that the program should
;; continue.
(defmethod rsb.formatting:format-event ((event  t)
                                        (style  style-monitor/object-tree)
                                        (target t)
                                        &key &allow-other-keys)
  t)

(defmethod rsb.formatting:format-event ((event  (eql :trigger))
                                        (style  style-monitor/object-tree)
                                        (target t)
                                        &key &allow-other-keys)
  (fresh-line target)
  (let+ (((&structure-r/o style- database max-depth) style))
    (print-object-tree database target max-depth)))

;;; `style-object-tree'

(defclass style-object-tree (database-mixin
                             object-tree-printing-mixin)
  ((delay :initarg  :delay
          :type     (or null positive-real)
          :reader   style-delay
          :documentation
          "Amount of time in seconds to wait before printing a
           snapshot of the introspection database."))
  (:default-initargs
   :delay 1.0)
  (:documentation
   "Quickly gather and print a snapshot of the introspection tree.

    The default behavior is gathering information for one second then
    printing the resulting snapshot. In most systems, one second
    should be enough to request and obtain all available introspection
    information. However, circumstances like degraded system
    performances or extreme communication latency may required longer
    delays."))

(service-provider:register-provider/class
 'style :object-tree :class 'style-object-tree)

(defmethod detach ((participant style-object-tree)))

;; Introspection event => ignore.
(defmethod rsb.ep:handle ((sink style-object-tree) (data list)))

;; Dummy event => print the object tree snapshot and return false to
;; indicate that the program should be terminated.
(defmethod rsb.formatting:format-event ((event  t)
                                        (style  style-object-tree)
                                        (target t)
                                        &key &allow-other-keys)
  (fresh-line target)
  (let+ (((&structure-r/o style- database max-depth delay) style))
    (when delay
      (sleep delay))
    (print-object-tree database target max-depth))
  nil)

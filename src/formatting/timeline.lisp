;;;; timeline.lisp --- Render a timeline view of received events.
;;;;
;;;; Copyright (C) 2012-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.formatting)

;;; Cache cell

(defstruct (%cell (:constructor %make-cell (lower upper)) (:copier nil))
  "The lower and upper slots indicate the temporal range of the cell.

   The count and max-size slots accumulate the respective quantity for
   events associated to the cell.

   The glyph slot is either nil if the cell has not been rendered yet
   or the rendered character for the cell."
  (lower    0   :type timestamp/unix/nsec :read-only t)
  (upper    0   :type timestamp/unix/nsec :read-only t)
  (count    0   :type non-negative-integer)
  (max-size 0   :type non-negative-integer)
  (glyph    nil :type (or null character)))

(defun cell-glyph (cell)
  (declare (type %cell cell))
  (or (%cell-glyph cell)
      (setf (%cell-glyph cell)
            (let+ (((&structure-r/o %cell- count max-size) cell))
              (glyph-for-data count max-size)))))

(defun cell-%update (cell events
                     &key
                     (start 0)
                     (end   (length events)))
  (declare (type %cell cell)
           (type sequence events))
  (let+ (((&structure %cell- count max-size glyph) cell)
         (new-count (- end start))
         (new-size  (reduce #'max events
                            :key           #'cdr
                            :initial-value 0
                            :start         start
                            :end           end)))
    (declare (type non-negative-integer new-count new-size))
    (incf count new-count)
    (maxf max-size new-size)
    (setf glyph nil)))

(defun glyph-for-data (count size)
  (declare (type non-negative-integer count size))

  (macrolet ((size-glyphs (small medium large)
               `(cond
                  ((<=     0 size  1024) ,small)
                  ((<   1024 size 65536) ,medium)
                  (t                     ,large))))
    (if *output-unicode?*
        (cond
          ((zerop count) #\Space)
          ((= count 1)   (size-glyphs #\· #\▪ #\◾))
          ((= count 2)   (size-glyphs #\╌ #\╍ #\╍))
          ((= count 3)   (size-glyphs #\┄ #\⋯ #\┅))
          (t             (size-glyphs #\─ #\━ #\▬)))
        (cond
          ((zerop count) #\Space)
          ((= count 1)   (size-glyphs #\. #\o #\O))
          ((= count 2)   (size-glyphs #\. #\o #\O))
          ((= count 3)   (size-glyphs #\- #\- #\=))
          (t             (size-glyphs #\- #\- #\=))))))

;;; `timeline' class

(defclass timeline (temporal-bounds-mixin
                    timestamp-mixin
                    width-specification-mixin
                    width-mixin)
  ((tic-distance :initarg  :tic-distance
                 :type     positive-integer
                 :accessor style-tic-distance
                 :initform 12
                 :documentation
                 "The distance in characters between tics in the
                  header line.")
   (events       :type     list
                 :accessor style-%events
                 :initform '()
                 :documentation
                 "Stores a list of events which have not been rendered
                  yet.

                  The list has to be sorted according to decreasing
                  timestamps.")
   (cache        :type     (or null (cons %cell t))
                 :accessor style-%cache
                 :initform nil
                 :documentation
                 "Stores information regarding previously rendered
                  cells. See `%timeline-cache-cell'."))
  (:default-initargs
   :upper-bound '(+ :now 1)
   :widths      '(:range 16))
  (:documentation
   "Instances of this column class render a timeline view in which
    received events appear as dots. A corresponding header with time
    tics can also be produced."))

(service-provider:register-provider/class
 'column :timeline :class 'timeline)

(defmethod column< ((left timeline) (right timeline))
  (let+ (((&flet earliest-event (timeline)
            (position-if #'plusp (style-%cache timeline)
                         :key #'%cell-count))))
    (value< (earliest-event left) (earliest-event right))))

(defmethod collects? ((style timeline))
  t)

(macrolet ((define-access?-method (part)
             `(defmethod rsb.ep:access? ((processor timeline)
                                         (part      (eql ,part))
                                         (mode      (eql :read)))
                t)))
  (define-access?-method :data) ; only if rsb.transport.payload-size meta-data item is not available
  (define-access?-method :meta-data)
  (define-access?-method :timestamp))

(defmethod format-header ((style timeline) (target t))
  (let+ (((&accessors-r/o
           ((&values (lower upper) now) bounds/expanded)
           (width        column-width)
           (tic-distance style-tic-distance))
          style)
         (tic-distance* (min tic-distance (1- width)))
         (delta         (/ (- upper lower)
                           (1- (/ width tic-distance*)))))
    (iter (for i :from (- upper now) :downto (- lower now) :by delta)
          (format target "~V@<~C~[ now~:;~:*~:/rsb.formatting:print-human-readable-duration/~]~>"
                  tic-distance*
                  (if *output-unicode?* #\↓ #\v)
                  (round i 1000000000)))))

(defmethod format-event ((event  (eql :trigger))
                         (style  timeline)
                         (target t)
                         &key)
  ;; Return early if the style is effectively disabled.
  (when (zerop (column-width style))
    (return-from format-event))

  ;; Add new cells to the cache as necessary and fill them.
  (adjust-cache! style)
  (fill-cache! style)

  ;; Copy cached characters into the output vector and write it out in
  ;; a single batch operation.
  (let+ (((&accessors-r/o (width column-width)
                          (cache style-%cache))
          style)
         (output (make-string width)))
    (declare (type list cache))
    (map-into output #'cell-glyph cache)
    (write-string output target)))

;; This method is necessary to satisfy the constraint that a primary
;; method has to exist but it should not actually get called.
(defmethod format-event ((event  t)
                         (style  timeline)
                         (target t)
                         &key)
  (error "Should not get called"))

(defmethod format-event :around ((event  t)
                                 (style  timeline)
                                 (target t)
                                 &key)
  ;; Collect non-trigger events into STYLE's buffer. Pass through
  ;; trigger events.
  (if (eq event :trigger)
      (call-next-method)
      (let+ (((&structure style- timestamp (events %events)) style)
             (entry (cons (funcall (the function timestamp) event)
                          (rsb.stats:event-size/power-of-2 event 0))))
        (setf events (merge 'list (list entry) events #'> :key #'car)))))

(defmethod adjust-cache! ((style timeline))
  (let+ (((&accessors-r/o ((lower-bound upper-bound) bounds/expanded)
                          (width                     column-width))
          style)
         ((&accessors (cache style-%cache)) style)
         (delta       (floor (- upper-bound lower-bound) width))
         (cache-upper (or (when-let ((first (first cache)))
                            (%cell-upper first))
                          lower-bound)))
    ;; Create new cells at the head of the cache.
    (iter (for upper :from    (+ cache-upper delta) :to upper-bound :by delta)
          (for lower previous upper                 :initially cache-upper)
          (push (%make-cell lower upper) cache))

    ;; Add and drop cells at the tail of the cache.
    (iter (repeat width)
          (for tail :first cache :then (cdr tail))
          (when (null (cdr tail))
            (let ((cell (%make-cell 0 0)))
              (setf (%cell-glyph cell) #\Space)
              (setf (cdr tail) (cons cell nil))))
          (finally (setf (cdr tail) nil)))))

(defmethod fill-cache! ((style timeline))
  (let+ (((&structure-r/o style- (events %events) (cache %cache))
          style))
    ;; Iterate over bins of the form [LOWER, UPPER] for all
    ;; not-yet-populated cache cells.
    (iter outer
          (generate events/rest on   events)
          (generate event       next (first (next events/rest)))
          (for      cells       on   cache)
          (for      cell        next (first cells))
          (while cell)

          ;; Advance to first event that is in the first bin.
          (when (first-iteration-p)
            (next event)
            (iter (until (<= (car event) (%cell-upper cell)))
                  (in outer (next event))))

          ;; Collect all events for the bin [LOWER, UPPER].
          (let+ (((&structure-r/o %cell- lower upper) cell))
            (iter (with events/bin                      = events/rest)
                  (with (the non-negative-fixnum count) = 0)
                  (while (<= lower (car event) upper))
                  (incf count)
                  (in outer (next event))
                  (finally-protected
                   (cell-%update cell events/bin :end count)))))

    ;; Drop events.
    (setf (style-%events style) '())))

;; Local Variables:
;; coding: utf-8
;; End:

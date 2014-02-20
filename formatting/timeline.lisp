;;;; timeline.lisp --- Render a timeline view of received events.
;;;;
;;;; Copyright (C) 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.formatting)

;;; Cache cell

(defstruct (%cell (:constructor %make-cell (lower upper)))
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

(defun %cell-update (cell events
                     &key
                     (start 0)
                     (end   (length events)))
  (declare (type %cell cell)
           (type sequence events))
  (let+ (((&structure %cell- count max-size glyph) cell)
         (new-count (- end start))
         (new-size  (reduce #'max events
                            :key           #'rsb.stats:event-size/power-of-2
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
    (if *textual-output-can-use-utf-8?*
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

(defmethod find-column-class ((spec (eql :timeline)))
  (find-class 'timeline))

(defclass timeline (temporal-bounds-mixin
                    timestamp-mixin
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
  (:documentation
   "Instances of this column class render a timeline view in which
    received events appear as dots. A corresponding header with time
    tics can also be produced."))

(defmethod collects? ((style timeline))
  t)

(defmethod format-header ((style  timeline)
                          (target t))
  (let+ (((&accessors-r/o
           ((&values (lower upper) now) bounds/expanded)
           (width        column-width)
           (tic-distance style-tic-distance)) style)
         (tic-distance* (min tic-distance (1- width)))
         (delta         (/ (- upper lower)
                           (1- (/ width tic-distance*)))))
    (iter (for i :from (- upper now) :downto (- lower now) :by delta)
          (format target "~V@<~C~[ now~:;~:*~5,1F s~]~>"
                  tic-distance*
                  (if *textual-output-can-use-utf-8?* #\↓ #\v)
                  (/ i 1000000000)))))

(defmethod format-event ((event  (eql :trigger))
                         (style  timeline)
                         (target t)
                         &key &allow-other-keys)
  ;; Add new cells to the cache as necessary and fill them.
  (adjust-cache! style)
  (fill-cache! style)

  ;; Copy cached characters into the output vector and write it out in
  ;; a single batch operation.
  (let+ (((&accessors-r/o (width column-width)
                          (cache style-%cache)) style)
         (output (make-string width)))
    (declare (type list cache))
    (map-into output #'cell-glyph cache)
    (write-string output target)))

(defmethod format-event ((event  t)
                         (style  timeline)
                         (target t)
                         &key &allow-other-keys)
  (error "Should not get called"))

(defmethod format-event :around ((event  t)
                                 (style  timeline)
                                 (target t)
                                 &key &allow-other-keys)
  ;; Collect non-trigger events into STYLE's buffer. Pass through
  ;; trigger events.
  (if (eq event :trigger)
      (call-next-method)
      (let+ (((&structure style- timestamp (events %events)) style))
        (setf events (merge 'list (list event) events #'>
                            :key timestamp)))))

(defmethod adjust-cache! ((style timeline))
  (let+ (((&accessors-r/o ((lower-bound upper-bound) bounds/expanded)
                          (width                     column-width)) style)
         ((&accessors (cache style-%cache)) style)
         (delta       (floor (- upper-bound lower-bound) width))
         (cache-upper (or (when-let ((first (first cache)))
                            (%cell-upper first))
                          lower-bound)))
    ;; Create new cells at the head of the cache.
    (iter (for upper :from    (+ cache-upper delta) :to upper-bound :by delta)
          (for lower previous upper                 :initially cache-upper)
          (push (%make-cell lower upper) cache))

    ;; Drop old cells at the tail of the cache.
    ;; TODO(jmoringe, 2012-03-21): slow
    (when-let ((tail (nthcdr width cache)))
      (setf (cdr tail) nil))))

(defmethod fill-cache! ((style timeline))
  (let+ (((&structure-r/o
           style- timestamp (events %events) (cache %cache)) style))
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
            (let ((key (funcall timestamp event)))
              (iter (until (<= key (%cell-upper cell)))
                    (in outer (next event)))))

          ;; Collect all events for the bin [LOWER, UPPER].
          (let+ (((&structure-r/o %cell- lower upper) cell))
            (iter (with events/bin                      = events/rest)
                  (with (the non-negative-fixnum count) = 0)
                  (while (<= lower (funcall timestamp event) upper))
                  (incf count)
                  (in outer (next event))
                  (finally-protected
                   (%cell-update cell events/bin :end count)))))

    ;; Drop events.
    (setf (style-%events style) '())))

;; Local Variables:
;; coding: utf-8
;; End:

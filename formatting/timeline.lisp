;;;; timeline.lisp --- Render a timeline view of received events.
;;;;
;;;; Copyright (C) 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.formatting)

;;; Cache type and functions

(deftype %timeline-cache-cell ()
  "Cache cell of the form

     ((LOWER . UPPER) . NULL-OR-CHARACTER)

   where LOWER and UPPER are `timestamp/unix/nsec' which indicate the
   temporal range of the cell and NULL-OR-CHARACTER is either nil of
   the cell has not been rendered yet or the rendered character for
   the cell."
  '(cons (cons timestamp/unix/nsec timestamp/unix/nsec)
         (or null character)))

(declaim (ftype (function (%timeline-cache-cell) t)
                %cell-lower %cell-upper %cell-value)
         (inline %make-cell %cell-lower %cell-upper %cell-value (setf %cell-value)))

(defun %make-cell (lower upper &optional value)
  (cons (cons lower upper) value))

(defun %cell-lower (cell)
  (car (car cell)))

(defun %cell-upper (cell)
  (cdr (car cell)))

(defun %cell-value (cell)
  (cdr cell))

(defun (setf %cell-value) (new-value cell)
  (setf (cdr cell) new-value))

;;; `timeline' class

(defmethod find-column-class ((spec (eql :timeline)))
  (find-class 'timeline))

(defclass timeline (temporal-bounds-mixin
                    width-mixin)
  ((tic-distance :initarg  :tic-distance
                 :type     positive-integer
                 :accessor style-tic-distance
                 :initform 12
                 :documentation
                 "The distance in characters between tics in the
                  header line.")
   (events       :initarg  :events
                 :type     list
                 :accessor style-events
                 :initform '()
                 :documentation
                 "Stores a list of events which have not been rendered
                  yet.")
   (cache        :type     (or null (cons %timeline-cache-cell t))
                 :accessor %style-cache
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
  ;; After copying, drop the newest cache cell to force recomputation
  ;; since its content is preliminary: more events for the cell may
  ;; arrive.
  (let+ (((&accessors-r/o (width column-width)
                          (cache %style-cache)) style)
         (output (make-string width)))
    (declare (type list cache))
    (map-into output #'%cell-value cache)
    (pop (%style-cache style))
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
      (push event (style-events style))))

(defmethod adjust-cache! ((style timeline))
  (let+ (((&accessors-r/o
           ((lower-bound upper-bound) bounds/expanded)
           (width                     column-width)) style)
         (delta       (floor (- upper-bound lower-bound) width))
         ((&accessors (cache %style-cache)) style)
         (cache-upper (or (when (first cache)
                            (%cell-upper (first cache)))
                          lower-bound)))
    ;; Create new cells at the head of the cache.
    (iter (for upper :from    (+ cache-upper delta) :to upper-bound :by delta)
          (for lower previous upper                 :initially cache-upper)
          (push (%make-cell lower upper) cache))

    ;; Drop old cells at the tail of the cache.
    ;;; TODO(jmoringe, 2012-03-21): slow
    (when-let ((tail (nthcdr width cache)))
      (setf (cdr tail) nil))))

(defmethod fill-cache! ((style timeline))
  (let+ (((&accessors-r/o (events style-events)
                          (cache  %style-cache)) style)
         ((&flet key (event)
            (timestamp->unix/nsecs (timestamp event :send)))) ; TODO(jmoringe, 2012-04-10): make configurable
         (events/cutoff)
         (cache/rest))
    ;; Iterate over bins of the form [LOWER, UPPER] for all
    ;; not-yet-populated cache cells.
    (iter outer
          (generate events/rest                 on events)
          (generate event                       next (car (next events/rest)))
          (for      cells                       on   cache)
          (for      cell                        next (first cells))
          (for      ((lower . upper) . content) next cell)
          (declare (type (or null %timeline-cache-cell) cell))
          (until content) ;; Stop at non-empty cache cell.

          ;; Advance to first event that is in the first bin.
          (when (first-iteration-p)
            (next event)
            (iter (until (<= (key event) upper))
                  (in outer (next event))))

          ;; Collect all events for the bin [LOWER, UPPER].
          (iter (with events/bin                      = events/rest)
                (with (the non-negative-fixnum count) = 0)
                (while (<= lower (key event) upper))
                (incf count)
                (in outer (next event))
                (finally-protected
                 (setf (%cell-value cell)
                       (glyph-for-events style events/bin :end count))))

          (setf events/cutoff events/rest)
          (finally-protected
           (unless (%cell-value cell)
             (setf (%cell-value cell) #\Space))
           (unless (%cell-value (second cells))
             (setf cache/rest (rest cells)))))

    ;; If there is some tail of the cache which we did not visit,
    ;; there will be no events for that part.
    (iter (for cell in cache/rest)
          (setf (%cell-value cell) #\Space))

    ;; If we did not visit all events, the remaining events can be
    ;; dropped, since they have already been processed or are outside
    ;; current and future bounds.
    (when events/cutoff
      (setf (rest events/cutoff) nil))))

(defmethod glyph-for-events ((style  timeline)
                             (events sequence)
                             &key
                             (start 0)
                             (end   (length events)))
  (let ((count (- end start))
        (size  (reduce #'max events
                       :key           #'rsb.stats:event-size/power-of-2
                       :initial-value 0
                       :start         start
                       :end           end)))
    (declare (type non-negative-integer start end size))

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
            (t             (size-glyphs #\- #\- #\=)))))))

;; Local Variables:
;; coding: utf-8
;; End:

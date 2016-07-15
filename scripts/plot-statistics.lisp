;;;; plot-statistics.lisp --- Plot timing-related statistics for multiple scopes.
;;;;
;;;; Copyright (C) 2013, 2014, 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

;;;; Requires Gnuplot version 4.6 or newer.

(unless (boundp 'initialized?)
  (setf (symbol-value 'initialized?) t
        *random-state*               (make-random-state t))

  (let+ (;; Tunable parameters
         (bucket-size        2) ; seconds
         (terminal-name      "pngcairo")
         (terminal-options   "enhanced font \"Verdana,12\" size 2000, 1500")
         (type               "png")
         ;; http://www.gnuplotting.org/ease-your-plotting-with-config-snippets/
         (borders
          (format nil "set border 0~@
                       set style line 101 lc rgb '#808080' lt 0 lw 2~@
                       set grid back ls 101"))
         ;; http://www.gnuplotting.org/tag/colormap/
         (colors             #+no '(#x0072bd #xd95319 #xedb120 #x7e2f8e
                                    #x77ac30 #x4dbeee #xa2142f))
         (palette
          (when colors
            (with-output-to-string (stream)
              (loop :for i :from 1 :to 100
                 :for color :in (apply #'circular-list colors)
                 :do (format stream "set style line ~D lt 1 lc rgb '#~6,'0X'~%"
                             i color)
                 :finally (write-string "set style increment user" stream)))))
         (legend-entry-limit 40)
         ;; Internal
         ((&flet random-name (type)
            (format nil "/tmp/~8,'0X.~A" (random (expt 16 8)) type)))
         (data-file   (random-name "txt"))
         (plot-file   (random-name "plt"))
         (scopes      (make-hash-table :test #'eq))
         (start-time  nil)
         (last-output (timestamp %event :receive)))

    (defun make-quatities ()
      (mapcar  #'rsb.stats:make-quantity
               `((:latency :from :create :to :send)
                 (:latency :from :send :to :receive)
                 (:latency :from :receive :to :deliver)
                 :rate
                 (:period-time :extractor ,(rcurry #'timestamp :create))
                 (:period-time :extractor ,(rcurry #'timestamp :send)))))

    (defun save-statistics (file timestamp scopes)
      (with-output-to-file (stream file :if-does-not-exist :create
                                        :if-exists         :append)
        (format stream "~A" timestamp)
        (mapcar (lambda+ ((&ign . (&ign . quantities)))
                  (mapcar (lambda (q)
                            (format stream "~{ ~F~}"
                                    (substitute
                                     #\- :n/a
                                     (multiple-value-list
                                      (rsb.stats:quantity-value q)))))
                          quantities))
                (sort scopes #'< :key #'cadr))
        (format stream "~%")))

    (defun process-event (event)
      (let+ ((scope               (intern-scope (event-scope event)))
             (timestamp           (timestamp event :receive))
             ((&ign . quantities) (ensure-gethash
                                   scope scopes
                                   (cons (hash-table-count scopes)
                                         (make-quatities)))))
        ;; Process EVENT in all quantities.
        (mapc (rcurry #'rsb.stats:update! event) quantities)
        ;; If we reached the end of the current bucket, output the
        ;; current values of all quantities and reset the quantities.
        (unless start-time
          (setf start-time timestamp))
        (when (> (local-time:timestamp-difference timestamp last-output)
                 bucket-size)
          ;; Drop buckets when the input data has obvious gaps.
          (let ((duration (local-time:timestamp-difference timestamp start-time)))
            (if (< duration (* 1.5 bucket-size))
                (save-statistics data-file start-time (hash-table-alist scopes))
                (warn "~@<Dropping bucket at ~A because of input gap of ~
                       ~~ ~,3F second~:P.~@:>"
                      timestamp duration)))

          (iter (for (_scope (_id . quantities)) in-hashtable scopes)
                (mapc #'rsb.stats:reset! quantities))
          (setf start-time  nil
                last-output timestamp))))

    (defun write-plot-commands-for-scope (stream scope index first?
                                          &key
                                          (which '(:send-receive)))
      (let ((which  (ensure-list which))
            (first? first?))
        (labels ((do-quantity (index)
                   (let ((title (ppcre:regex-replace-all "_" scope "\\\\\\\\_")))
                     (format stream "~:[, \\~%\"\"~:;~] u 1:~D w l title \"~A\""
                             first? index title))
                   (setf first? nil))
                 (maybe-do-quantity (index name)
                   (when (member name which :test #'eq)
                     (do-quantity index))))

          (maybe-do-quantity (+ index 0) :create-send)
          (maybe-do-quantity (+ index 2) :send-receive)
          (maybe-do-quantity (+ index 4) :receive-deliver)
          (maybe-do-quantity (+ index 6) :rate)
          (maybe-do-quantity (+ index 7) :period-time/create)
          (maybe-do-quantity (+ index 9) :period-time/send))))

    (defun write-plot-commands (scopes output script
                                &key
                                (which :send-receive))
      (with-output-to-file (stream script :if-does-not-exist :create
                                          :if-exists         :supersede)
        (format stream "set terminal ~A ~@[~A~]
                        set output \"~A.~A\"

                        ~@[~A~]

                        ~A

                        set xdata time
                        set timefmt \"%Y-%m-%dT%H:%M:%S+01:00\"

                        set xtics font \",10\"
                        set xtics rotate by -30

                        set format y \"%.2s %cs\"
                        set ytics font \",10\"

                        ~:[~
                          unset key
                        ~:;~
                          set key outside above
                        ~]

                        plot ~S"
                terminal-name terminal-options output type
                palette borders
                (< (length scopes) legend-entry-limit) data-file)
        (iter (for (scope . (id . _)) :in    (sort scopes #'string<
                                                  :key (compose #'scope-string #'car)))
              (for first?             :first t :then nil)
              (write-plot-commands-for-scope
               stream (scope-string scope) (+ 2 (* 11 id)) first? :which which))))

    (push (lambda ()
            (let+ (((&flet do-plot (name which)
                      (write-plot-commands (hash-table-alist scopes) name plot-file
                                           :which which)
                      (sb-ext:run-program "gnuplot" (list plot-file)
                                          :search t
                                          :error  *error-output*)
                      #+no (delete-file plot-file))))

              (mapc (lambda (which)
                      (do-plot (substitute
                                #\- #\/
                                (format nil "~@[~A-~]~(~A~)" nil which))
                        which))
                    '(:create-send :send-receive :rate
                      :period-time/create :period-time/send))
              #+no (when (probe-file data-file) (delete-file data-file))))
          sb-ext:*exit-hooks*)))

(process-event %event)

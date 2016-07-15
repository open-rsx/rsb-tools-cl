;;;; plot-timestamps.lisp --- Plot timestamps for multiple scopes.
;;;;
;;;; Copyright (C) 2013, 2014, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

;;;; Requires Gnuplot version 4.6 or newer.

(unless (boundp 'initialized?)
  (setf (symbol-value 'initialized?) t
        *random-state*               (make-random-state t))

  (let+ (;; Tunable parameters
         (timestamps         '(:create :send :receive :deliver))
         (terminal-name      "pngcairo")
         (terminal-options   "enhanced font \"Verdana,12\" size 2000, 1500")
         (type               "png")
         ;; http://www.gnuplotting.org/ease-your-plotting-with-config-snippets/
         (borders
          (format nil "set border 0~@
                       set style line 101 lt 0 lw 2 lc rgb '#808080'~@
                       set grid back ls 101"))
         ;; http://www.gnuplotting.org/tag/colormap/
         (colors             (circular-list #x0072bd #xd95319 #xedb120
                                            #x7e2f8e #x77ac30 #x4dbeee
                                            #xa2142f))
         (legend-entry-limit 40)

         ;; Internal
         ((&flet random-name (type)
            (format nil "/tmp/~8,'0X.~A" (random (expt 16 8)) type)))
         (plot-file (random-name "plt"))
         (scopes    (make-hash-table :test #'eq)))

    (defun make-color (index1 index2)
      (let+ ((color (nth index1 colors))
             ((&flet do-channel (channel)
                (let* ((offset (* 8 channel))
                       (value  (ldb (byte 8 offset) color)))
                  (ash (floor (lerp (/ (1- index2) 6) value #xff)) offset)))))
        (logior (do-channel 0) (do-channel 1) (do-channel 2))))

    (defun open-data-file ()
      (let ((data-file (random-name "txt")))
        (values data-file (open data-file
                                :if-does-not-exist :create
                                :if-exists         :supersede
                                :direction         :output))))

    (defun process-event (event)
      (let+ ((scope      (intern-scope (event-scope event)))
             (timestamps (mapcar (curry #'timestamp event) timestamps))
             ((&ign stream) (ensure-gethash
                             scope scopes
                             (multiple-value-list (open-data-file))))
             (*print-pretty* nil))
        (format stream "~{~A~^, ~}~%" timestamps)))

    (defun write-plot-commands-for-scope (stream scope file timestamps index first?)
      (let+ ((first? first?)
             ((&flet do-timestamp (index index2 name)
                (let* ((scope (scope-string scope))
                       (scope (ppcre:regex-replace-all "_" scope "\\\\\\\\_"))
                       (y     (- (* 0.1 (1+ (length timestamps)) index)
                                 (* 0.1 index2))))
                  (format stream "~:[, \\~%~:;~]~S ~
                                  u ~D:(~F)~[~2*~;:yticlabel(\"~A ~A\")~:;~2*~] ~
                                  pt 7 ps .3 lt rgb \"#~6,'0X\" notitle"
                          first? file
                          index2 y index2 scope name
                          (make-color index index2)))
                (setf first? nil))))
        (iter (for timestamp in    timestamps)
              (for index2    :from 1)
              (do-timestamp index index2 timestamp))))

    (defun write-plot-commands (scopes output script)
      (with-output-to-file (stream script
                                   :if-does-not-exist :create
                                   :if-exists         :supersede)
        (format stream "set terminal ~A~@[ ~A~]
                        set output \"~A.~A\"

                        ~A

                        set xdata time
                        set timefmt \"%Y-%m-%dT%H:%M:%S+01:00\"

                        set xtics font \",10\"
                        set xtics rotate by -30

                        ~:[~
                          unset ytics
                        ~:;~
                          set ytics font \",10\"
                        ~]

                        plot "
                terminal-name terminal-options output type borders
                (< (hash-table-count scopes) legend-entry-limit))
        (iter (for  (scope . (file)) in     (sort (hash-table-alist scopes) #'string<
                                                  :key (compose #'scope-string #'car)))
              (for  index            :from  0)
              (for  first?           :first t :then nil)
              (write-plot-commands-for-scope stream scope file timestamps index first?))))

    (push (lambda ()
            (mapc (compose #'close #'second) (hash-table-values scopes))
            (write-plot-commands scopes "timestamps" plot-file)
            (sb-ext:run-program "gnuplot" (list plot-file)
                                :search t
                                :error  *error-output*)
            (mapc (compose #'delete-file #'first) (hash-table-values scopes))
            (delete-file plot-file))
          sb-ext:*exit-hooks*)))

(process-event %event)

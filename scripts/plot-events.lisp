;;;; plot-events.lisp --- Plot events for multiple scopes.
;;;;
;;;; Copyright (C) 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

;;;; Requires Gnuplot version 4.6 or newer.

(unless (boundp 'initialized?)
  (setf (symbol-value 'initialized?) t
        *random-state*               (make-random-state t))

  (let+ (;; Tunable parameters
         (timestamp        :receive)
         (terminal-name    "pngcairo")
         (terminal-options "enhanced font \"Verdana,12\" size 2000, 1500")
         (type             "png")
         ;; http://www.gnuplotting.org/ease-your-plotting-with-config-snippets/
         (borders
          (format nil "set border 0~@
                       set style line 101 lt 0 lw 2 lc rgb '#808080'~@
                       set grid back ls 101"))
         ;; http://www.gnuplotting.org/tag/colormap/
         (colors           #+no '(#x0072bd #xd95319 #xedb120 #x7e2f8e
                                  #x77ac30 #x4dbeee #xa2142f))
         (palette
          (when colors
            (with-output-to-string (stream)
              (loop :for i :from 1 :to 100
                 :for color :in (apply #'circular-list colors)
                 :do (format stream "set style line ~D lt 1 lc rgb '#~6,'0X'~%"
                             i color)
                 :finally (write-string "set style increment user" stream)))))
         (tick-count-limit 100)
         ;; Internal
         ((&flet random-name (type)
            (format nil "/tmp/~8,'0X.~A" (random (expt 16 8)) type)))
         (plot-file (random-name "plt"))
         (scopes    (make-hash-table :test #'eq)))

    (defun open-data-file ()
      (let ((data-file (random-name "txt")))
        (values data-file (open data-file
                                :if-does-not-exist :create
                                :if-exists         :supersede
                                :direction         :output))))

    (defun process-event (event)
      (let+ ((scope      (intern-scope (event-scope event)))
             (timestamp  (timestamp event timestamp))
             ((&ign stream) (ensure-gethash
                             scope scopes
                             (multiple-value-list (open-data-file))))
             (*print-pretty* nil))
        (format stream "~A, ~A~%" timestamp (event-size event))))

    (defun write-plot-commands-for-scope (stream scope file timestamps index first? tics?)
      (let* ((scope (scope-string scope))
             (title (ppcre:regex-replace-all "_" scope "\\\\\\\\_")))
        (format stream "~:[, \\~%~:;~]~S ~
                        u ~D:(~F):(log10(3+$~D/100))~:[~:;:yticlabel(\"~A\")~] ~
                        pt 7 ps variable notitle"
                first? file 1 index 2 tics? title)))

    (defun write-plot-commands (scopes output script)
      (with-output-to-file (stream script
                                   :if-does-not-exist :create
                                   :if-exists         :supersede)
        (let* ((scope-count  (hash-table-count scopes))
               (tick-divisor (max 1 (ceiling scope-count tick-count-limit))))
          (format stream "set terminal ~A~@[ ~A~]
                          set output \"~A.~A\"

                          ~@[~A~]

                          ~A

                          set xdata time
                          set timefmt \"%Y-%m-%dT%H:%M:%S+01:00\"

                          set xtics font \",10\"
                          set xtics rotate by -30
                          set ytics font \",10\"

                          set title \"Events by ~(~A~) timestamp\"

                          plot "
                  terminal-name terminal-options output type
                  palette borders timestamp)
          (iter (for  (scope . (file)) in     (sort (hash-table-alist scopes) #'string<
                                                    :key (compose #'scope-string #'car)))
                (for  index            :from  0)
                (for  first?           :first t :then nil)
                (write-plot-commands-for-scope
                 stream scope file timestamp index first?
                 (zerop (mod index tick-divisor)))))))

    (push (lambda ()
            (mapc (compose #'close #'second) (hash-table-values scopes))
            (write-plot-commands scopes "events" plot-file)
            (sb-ext:run-program "gnuplot" (list plot-file)
                                :search t
                                :error  *error-output*)
            (mapc (compose #'delete-file #'first) (hash-table-values scopes))
            (delete-file plot-file))
          sb-ext:*exit-hooks*)))

(process-event %event)

;;;; plot-timestamps.lisp --- Plot timestamps for multiple scopes.
;;;;
;;;; Copyright (C) 2013, 2014, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

;;;; Requires Gnuplot version 4.6 or newer.

(unless (boundp 'initialized?)
  (setf (symbol-value 'initialized?) t
        *random-state*               (make-random-state t))

  (let (;; Tunable parameters
        (timestamps       '(:create :send :receive :deliver))
        (colors           (circular-list #xff0000 #x00ff00 #x0000ff
                                         #xffff00 #xff00ff #x00ffff
                                         #x80ff00 #xff8000
                                         #x8000ff #xff0080
                                         #x0080ff #x00ff80))
        (type             "png")
        (terminal-options "font \",8\" size 1400, 800")
        ;; Internal variables
        (plot-file        (format nil "/tmp/~8,'0X.plt" (random (expt 16 8))))
        (scopes           (make-hash-table :test #'eq)))

    (defun make-color (index1 index2)
      (let ((base  (nth index1 colors))
            (white #xffffff))
        (floor (lerp (/ (1- index2) 5) base white))))

    (defun open-data-file ()
      (let ((data-file (format nil "/tmp/~8,'0X.txt" (random (expt 16 8)))))
        (values
         data-file
         (open data-file :if-does-not-exist :create
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
                (format stream "~:[, \\~%~:;~]~S u ~D:(~F) pt 5 ps .2 lt rgb \"#~6,'0X\" ~[~;title \"~A ~A\"~:;notitle~]"
                        first? file index2 (- (* 0.1 (1+ (length timestamps)) index)
                                              (* 0.1 index2))
                        (make-color index index2)
                        index2 (scope-string scope) name)
                (setf first? nil))))
        (iter (for timestamp in    timestamps)
              (for index2    :from 1)
              (do-timestamp index index2 timestamp))))

    (defun write-plot-commands (scopes output script)
      (with-output-to-file (stream script :if-does-not-exist :create
                                          :if-exists         :supersede)
        (format stream "set terminal ~A~@[ ~A~]
                        set output \"~A.~3:*~A\"~2*

                        set xdata time
                        set timefmt \"%Y-%m-%dT%H:%M:%S+01:00\"

                        set xtics font \",6\"
                        set xtics rotate by -30
                        unset ytics

                        set offsets 0, 0, .1, .1

                        set key outside above
                        set key invert
                        set key box

                        set grid

                        plot "
                type terminal-options output)
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

(process-event event)

;; sbcl --noinform --no-userinit --load ~/.local/share/common-lisp/quicklisp/setup.lisp --script make.lisp

(cl:defpackage #:rsb-tools-build (:use #:cl))
(cl:in-package #:rsb-tools-build)

(defun output (format-control format-arguments finishedp)
  (apply #'format t format-control format-arguments)
  (if finishedp (terpri) (finish-output)))

(defun l (format-control &rest format-arguments)
  (fresh-line)
  (output format-control format-arguments t))

(defun start (format-control &rest format-arguments)
  (fresh-line)
  (output format-control format-arguments nil))

(defun done (format-control &rest format-arguments)
  (output format-control format-arguments t))

(defun call-with-logged-action (format-control format-arguments thunk)
  (let ((start                (get-internal-real-time))
        (real-standard-output *standard-output*)
        (output               (make-string-output-stream))
        (error                (make-string-output-stream)))
    (apply #'start format-control format-arguments)
    (unwind-protect
         (handler-bind ((serious-condition
                         (lambda (condition)
                           (let ((*standard-output* real-standard-output))
                             (done "~48T❌~%~@<  | ~@;~A~@:>" condition)))))
           (multiple-value-prog1
               (let ((*standard-output* output)
                     (*error-output*    error))
                 (funcall thunk))
             (let* ((end      (get-internal-real-time))
                    (duration (/ (- end start) internal-time-units-per-second)))
               (done "~48T✓ (~,2F s)" duration))))
      (let ((error-output (get-output-stream-string error)))
        (when (plusp (length error-output))
          (format real-standard-output "~@<  | ~@;~A~@:>~%" error-output))))))

(defmacro with-logged-action ((format-control &rest format-arguments)
                              &body body)
  `(call-with-logged-action ,format-control (list ,@format-arguments)
                            (lambda () (progn ,@body))))

(with-logged-action ("Loading build dependencies…")
  (ql:quickload '(:alexandria :let-plus :inferior-shell :cl-ppcre) :silent t))

(use-package '#:alexandria)
(use-package '#:let-plus)

(defmacro with-suppressed-output ((&optional (stream '(make-broadcast-stream)))
                                  &body body)
  (with-unique-names (stream-var)
    `(let* ((,stream-var       ,stream)
            (*standard-output* ,stream-var)
            (*error-output*    ,stream-var))
       ,@body)))

(defun git-describe (directory &key match)
  (when (probe-file "temp")
    (delete-file "temp"))
  (close (open "temp" :if-does-not-exist :create))
  (inferior-shell:run/nil
   `("git" "describe" "--long" "--dirty=-dirty" "--tags"
           ,@(when match (list "--match" match)))
   :directory directory
   :output    #P"temp")
  (alexandria:read-file-into-string "temp"))

(defun project-version (directory system
                        &key
                        (system-package system))
  (unwind-protect
       (let+ ((system-file (make-pathname :name     (string-downcase system)
                                          :type     "asd"
                                          :defaults directory))
              ((&flet variable-value (name)
                 (symbol-value (find-symbol (string name) system-package))))
              (version-major    (progn
                                  (asdf/defsystem::load-asd system-file)
                                  (variable-value '#:+version-major+)))
              (version-minor    (variable-value '#:+version-minor+))
              (previous-release (format nil "release-~D.~D" version-major (1- version-minor)))
              (version-string   (git-describe directory :match previous-release)))
         (ppcre:register-groups-bind ((#'parse-integer version-revision) commit dirty)
             ("^release-[0-9]+\\.[0-9]+-([0-9]+)-(g[a-z0-9]+)(?:-(dirty))?" version-string)
           (values version-major version-minor version-revision commit dirty)))
    (asdf:clear-system system)))

(defun build (&key
              (directory           *default-pathname-defaults*)
              (system              :cl-rsb)
              (system-package      (symbolicate :system '#:-system))
              dependency-directory
              protocol-directory
              features)
  (let+ (((&values version-major version-minor version-revision commit dirty)
          (with-logged-action ("Determining project version…")
            (project-version directory system :system-package system-package))))
    ;;
    (l "This is ~(~A~) ~D.~D-~D-~A~@[-~A~]"
       system version-major version-minor version-revision commit dirty)
    (l "~{~:[-~;+~] ~A~^~%~}"
       `(protocol-directory ,protocol-directory
         ,@(mapcan #'list (circular-list t) features)))

    ;;
    (with-output-to-file (stream (merge-pathnames "version.sexp" directory)
                                 :if-does-not-exist :create
                                 :if-exists         :supersede)
      (format stream "~D ~A~%" version-minor version-revision))
    (defvar cl-user::*rsb.protocol-directory* protocol-directory)

    ;;
    (unwind-protect
         (progn
           (with-logged-action ("Configuring source registry…")
             (asdf:initialize-source-registry
              `(:source-registry
                (:directory ,directory)
                ,@(when dependency-directory
                    `((:tree ,dependency-directory)))
                :ignore-inherited-configuration)))

           (l "Loading features")
           (dolist (system features)
             (with-logged-action ("• ~A…" system)
               (ql:quickload system :silent t))) ; TODO log to build.log?

           ;; TODO source-registry
           ;; TODO optional systems
           (with-logged-action ("Building binary…")
	     (asdf:operate 'asdf:program-op system)
             #+no (let* ((file-stream   (open "build.log"
                                         :direction         :output
                                         :if-does-not-exist :create
                                         :if-exists         :supersede))
                    (string-stream (make-string-output-stream)))
	       (trace asdf:operate)
               (unwind-protect-case ()
                   (with-suppressed-output ((make-broadcast-stream
                                             file-stream string-stream))
                     (asdf:operate 'asdf:program-op system))
                 (:abort
		  (print :abort)
                  (format t "~@<| ~@;~A~@:>"
                          (get-output-stream-string string-stream)))
                 (:always
                  (close file-stream))))))
      (asdf:clear-source-registry)
      (asdf:clear-output-translations))))

(let+ (((dependency-directory protocol-directory) (uiop:command-line-arguments)))
  (build :directory            (namestring *default-pathname-defaults*)
         :system               :cl-rsb-tools-main
         :system-package       :cl-rsb-tools-main-system
         :protocol-directory   protocol-directory
         :dependency-directory dependency-directory
         :features             '(:rsb-builder

                                 :rsb-converter-protocol-buffer

                                 :rsb-filter-regex
                                 :rsb-filter-xpath

                                 :rsb-transport-socket
                                 #+no :rsb-transport-spread
                                 #+no :rsb-yarp

                                 #+no :rsb-tools-commands-web

                                 :cl-rsb-formatting-png
                                 #+no :cl-rsb-formatting-graph
                                 :rsb-formatting-and-rsb-stats)))

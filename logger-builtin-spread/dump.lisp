(load-system :asdf-system-connections)

(load-system :cl-ppcre) ;; for regex filter

(load-system :cl-spread) ;; for spread transport

(load-system :swank) ;; for the lulz

(load-system :cl-rsb-tools-logger)


(asdf:clear-source-registry)
(asdf:clear-output-translations)

(cffi:close-foreign-library 'spread::libspread)

(defun reload-spread-and-main ()
  (cffi:use-foreign-library spread::libspread)
  (rsb.tools.logger:main))

(com.dvlsoft.clon:dump "logger-builtin-spread" reload-spread-and-main
		       :compression :best)

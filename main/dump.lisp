(load-system :asdf-system-connections)

(load-system :cl-ppcre)          ;; for regex filter

(load-system :cl-protobuf)
#-win32 (load-system :cl-spread) ;; for spread transport

(load-system :usocket)           ;; for socket transport

(load-system :swank)             ;; for the lulz

(load-system :cl-rsb-tools-main)

(asdf:clear-source-registry)
(asdf:clear-output-translations)
#-win32 (cffi:close-foreign-library 'spread::libspread)

(defun reload-spread-and-main ()
  #-win32
  (handler-case
      (cffi:use-foreign-library spread::libspread)
    (error (condition)
      (warn "~@<Failed to load Spread library: ~A. Did you set ~
LD_LIBRARY_PATH? ~_Spread transport will now be disabled.~@:>"
	    condition)))
  (rsb.tools.main:main))

(com.dvlsoft.clon:dump "tools" reload-spread-and-main
		       #+sb-core-compression :compression
		       #+sb-core-compression 9)

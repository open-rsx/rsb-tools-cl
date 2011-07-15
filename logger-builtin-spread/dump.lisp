(load-system :cl-protobuf)
(load-system :cl-spread)
(load-system :cl-rsb-tools-logger)

(cffi:close-foreign-library 'spread::libspread)

(defun reload-spread-and-main ()
  (cffi:use-foreign-library spread::libspread)
  (rsb.tools.logger:main))

(com.dvlsoft.clon:dump "logger-builtin-spread" reload-spread-and-main)

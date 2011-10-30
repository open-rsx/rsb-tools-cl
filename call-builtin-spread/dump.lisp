(load-system :cl-protobuf)
(load-system :cl-spread)

(load-system :cl-rsb-tools-call)

(asdf:clear-source-registry)
(asdf:clear-output-translations)

(cffi:close-foreign-library 'spread::libspread)

(defun reload-spread-and-main ()
  (cffi:use-foreign-library spread::libspread)
  (rsb.tools.call:main))

(com.dvlsoft.clon:dump "call-builtin-spread" reload-spread-and-main
		       :compression :best)

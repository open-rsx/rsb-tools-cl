(load-system :asdf-system-connections)

(load-system :cl-rsb-tools-logger)

(asdf:clear-source-registry)
(asdf:clear-output-translations)

(com.dvlsoft.clon:dump "logger" rsb.tools.logger:main
		       :compression :best)

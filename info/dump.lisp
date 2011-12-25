(load-system :asdf-system-connections)

(load-system :cl-rsb-tools-info)

(asdf:clear-source-registry)
(asdf:clear-output-translations)

(com.dvlsoft.clon:dump "info" rsb.tools.info:main
		       :compression :best)

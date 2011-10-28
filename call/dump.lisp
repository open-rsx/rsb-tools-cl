(load-system :cl-rsb-tools-call)

(asdf:clear-source-registry)
(asdf:clear-output-translations)

(com.dvlsoft.clon:dump "call" rsb.tools.call:main
		       :compression :best)

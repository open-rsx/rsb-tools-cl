(load-system :asdf-system-connections)

(setf iterate::*always-declare-variables* t)

(load-system :cl-ppcre)          ;; for regex filter

(load-system :cl-protobuf)
#-win32 (load-system :network.spread) ;; for spread transport

(load-system :usocket)           ;; for socket transport

(load-system :swank)             ;; for the lulz

(load-system :cl-rsb-tools-main)

;;; Image saving/resuming logistics

;; Reset ASDF state
(asdf:clear-source-registry)
(asdf:clear-output-translations)

;; Try to reload Spread library
(network.spread::enable-reload-spread-library :if-fails #'warn)

(com.dvlsoft.clon:dump "tools" rsb.tools.main:main
		       :compression :best)

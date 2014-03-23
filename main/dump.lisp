(load-system :iterate)
(setf iterate::*always-declare-variables* t)

(load-system :cl-ppcre)               ; for regex filter

(load-system :cl-protobuf)
#-win32 (load-system :network.spread) ; for spread transport

(load-system :usocket)                ; for socket transport

(load-system :swank)                  ; for the lulz

(load-system :cl-rsb-tools-main)

;;; Image saving/resuming logistics

;; Reset ASDF state
(asdf:clear-source-registry)
(asdf:clear-output-translations)

;; Reseed RSB id generator
(rsb:enable-id-random-state-reseed)

;; As a default, try reloading foreign libraries on startup. If
;; necessary, users can change this by "redumping".
(rsb.tools.main:make-dynamic)

(com.dvlsoft.clon:dump
 #-win32 "tools" #+win32 "tools.exe"
 rsb.tools.main:main
 #+sb-core-compression :compression #+sb-core-compression 9)

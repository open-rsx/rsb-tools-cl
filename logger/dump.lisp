(ql:quickload :rsb-logger)
#+sbcl (setf sb-ext:*invoke-debugger-hook*
	     (lambda (condition previous-value)
	       (declare (ignore previous-value))
	       (format *error-output* "~A~%" condition)
	       (com.dvlsoft.clon:exit 1)))
(com.dvlsoft.clon:dump "logger" rsb-logger::main)

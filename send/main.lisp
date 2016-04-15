;;;; main.lisp --- Entry point of the send tool.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsb.tools.send)

(defun make-help-string (&key
                         (show :default))
  "Return a help that explains the commandline option interface."
  (with-output-to-string (stream)
    (format stream "Send an event constructed according to EVENT-SPEC ~
                    to listeners on scopes specified by ~
                    DESTINATION-URI.~@
                    ~@
                    If EVENT-SPEC is the empty string, an event ~
                    without payload is sent.~@
                    ~@
                    If EVENT-SPEC is one of the strings \"true\" and ~
                    \"false\", it is parsed as the corresponding ~
                    Boolean value.~@
                    ~@
                    EVENT-SPEC is parsed as string when surrounded ~
                    with double-quotes and as integer or float number ~
                    when consisting of digits without and with decimal ~
                    point respectively.~@
                    ~@
                    EVENT-SPEC is parsed as a scope when starting with ~
                    the \"/\" character.~@
                    ~@
                    If EVENT-SPEC is the single character \"-\" or the ~
                    string \"-:binary\", the entire \"contents\" of ~
                    standard input (until end of file) is read as a ~
                    string or octet-vector respectively and used as ~
                    argument for the method send.~@
                    ~@
                    If EVENT-SPEC is of one the forms #P\"PATHNAME\", ~
                    #P\"PATHNAME\":ENCODING or #P\"PATHNAME\":binary, ~
                    the file designated by PATHNAME is read into a ~
                    string (optionally employing ENCODING) or ~
                    octet-vector and sent.~@
                    ~@
                    If EVENT-SPEC is of the form ~
                    pb:.MESSAGE-TYPE-NAME:{FIELDS}, a protocol buffer ~
                    message of type MESSAGE-TYPE-NAME is constructed ~
                    and its fields are populated according to ~
                    FIELDS. FIELDS uses the syntax produced/consumed ~
                    by the various TextFormat classes of the protocol ~
                    buffer API and the --decode/--encode options of ~
                    the protoc binary.~@
                    ~@
                    If EVENT-SPEC is of one the forms ~
                    pb:.MESSAGE-TYPE-NAME:#P\"PATHNAME\" or ~
                    pb:.MESSAGE-TYPE-NAME:#P\"PATHNAME\":{ENCODING}, a ~
                    protocol buffer message of type MESSAGE-TYPE-NAME ~
                    is constructed according to the contents of the ~
                    file designated by PATHNAME.~@
                    ~@
                    Note that, when written as part of a shell ~
                    command, some of the above forms may require ~
                    protection from processing by the shell, usually ~
                    by surrounding the form in single quotes (').~@
                    ~@
                    DESTINATION-URI designates the destination scope ~
                    to which the event should be sent and the ~
                    transport configuration which should be used for ~
                    sending the event.~@
                    ~@
                    ")
    (with-abbreviation (stream :uri show)
      (progn
        (format stream "A DESTINATION-URI is of the form~@
                        ~@
                        ~2@T")
        (print-uri-help stream :uri-var "DESTINATION-URI")))))

(defun make-examples-string (&key (program-name "rsb send"))
  "Make and return a string containing usage examples of the program."
  (format nil
          "~2@T~A '' /mycomponent/trigger~@
           ~@
           Send an event without a payload to the channel designated ~
           by the scope /mycomponent/trigger.~@
           ~@
           Note the use of single quotes (') to allow specifying an ~
           empty payload.~@
           ~@
           ~2@T~:*~A '\"running\"' 'spread:/mycomponent/state'~@
           ~@
           Send an event whose payload is the string \"running\" to ~
           the channel designated by the scope /mycomponent/state.~@
           ~@
           Note the use of single quotes (') to prevent the shell from ~
           processing the double quotes (\") that identify the payload ~
           as a string.~@
           ~@
           ~2@T~:*~A 5 'spread:/somescope?name=4803'~@
           ~@
           Send an integer. Use spread transport, like in the previous ~
           example, but use the \"daemon name\" option of the Spread ~
           transport instead of specifying host and port.~@
           ~@
           Note the use of single quotes (') to prevent elements of ~
           the destination URI from being processed by the shell (not ~
           necessary for all shells).~@
           ~@
           ~2@Tcat my-data.txt | ~:*~A -- - 'socket:/printer'~@
           ~2@Tcat my-data.txt | ~:*~A -- -:binary 'socket:/printer'~@
           ~2@T~:*~A '#P\"my-data.txt\"' 'socket:/printer'~@
           ~2@T~:*~A '#P\"my-data.txt\":latin-1' 'socket:/printer'~@
           ~2@T~:*~A '#P\"my-data.txt\":binary' 'socket:/printer'~@
           ~@
           Several ways of sending the content of the file ~
           \"my-data.txt\" to the scope \"/printer\" using the socket ~
           transport (with its default configuration). These forms can ~
           only be used for sending string and octet-vector ~
           payloads.~@
           ~@
           Note the use of single quotes (') to prevent elements of ~
           the pathname #P\"my-data.txt\" from being processed by the ~
           shell.~@
           ~@
           ~2@T~:*~A \\~@
           ~4@T-IPATH-TO-RST/proto/stable/ \\~@
           ~4@T-lPATH-TO-RST/proto/stable/rst/robot/RobotCollision.proto \\~@
           ~4@T'pb:.rst.robot.RobotCollision:{kind: \"SELF\" ~
           collision_detail: { geometry: { contact_points: [ { x: 0 y: ~
           1 z: 2 frame_id: \"foo\" }, { x: 3 y: 4 z: 5 } ] } ~
           object_1: \"o1\" } }'~@
           ~4@T'socket:/collisions'~@
           ~@
           Send a protocol buffer message to scope /collisions. The ~
           protocol buffer message is of type rst.robot.RobotCollision ~
           with kind enum field set to SELF and an embedded ~
           rst.kinematics.ObjectCollision message with two contact ~
           points in the collision_detail field.~@
           ~@
           The specification of the message content uses the syntax ~
           produced/consumed by the various TextFormat classes of the ~
           protocol buffer API and the --decode/--encode options of ~
           the protoc binary.~@
           ~@
           Note how the definition of the protocol buffer message type ~
           is loaded using -I and -l commandline options.~@
           ~@
           "
          program-name))

(defun update-synopsis (&key
                        (show         :default)
                        (program-name "rsb send"))
  "Create and return a commandline option tree."
  (make-synopsis
   ;; Basic usage and specific options.
   :postfix "EVENT-SPEC [DESTINATION-URI]"
   :item    (make-text :contents (make-help-string :show show))
   :item    (make-common-options :show show)
   :item    (make-error-handling-options :show show)
   :item    (defgroup (:header "Event Options"
                       :hidden (not (show-help-for? '(:event)
                                                    :default t
                                                    :show    show)))
              (stropt :long-name       "method"
                      :short-name      "m"
                      :argument-name   "METHOD"
                      :description
                      "Set the method field of the event being sent to METHOD. Default behavior is sending an event without method field.")
              (stropt :long-name       "meta-data"
                      :short-name      "D"
                      :argument-name   "NAME=VALUE"
                      :description
                      "Set the meta-data item NAME to VALUE in the event being sent. This option can be specified multiple times for distinct NAMEs.")
              (stropt :long-name       "timestamp"
                      :short-name      "T"
                      :argument-name   "NAME=YYYY-MM-DD[THH:MM:SS[.µµµµµµ[+ZH:ZM]]]"
                      :description
                      "Set the timestamp named NAME to the timestamp YYYY-MM-DD[THH:MM:SS[.µµµµµµ[+ZH:ZM]]] in the event being sent. This option can be specified multiple times for distinct NAMEs.")
              (stropt :long-name       "cause"
                      :short-name      "c"
                      :argument-name   "PARTICIPANT-ID:SEQUENCE-NUMBER"
                      :description
                      "Add the event id described by PARTICIPANT-ID:SEQUENCE-NUMBER to the cause vector of the event being sent. This option can be specified multiple times."))
   ;; Append IDL options
   :item    (make-idl-options)
   ;; Append RSB options.
   :item    (make-options
             :show? (or (eq show t)
                        (and (listp show) (member :rsb show))))
   ;; Append examples.
   :item    (defgroup (:header "Examples")
              (make-text :contents (make-examples-string
                                    :program-name program-name)))))

(defun main (program-pathname args)
  "Entry point function of the cl-rsb-tools-send system."
  (let ((program-name (concatenate
                       'string (namestring program-pathname) " send")))
    (update-synopsis :program-name program-name)
    (setf *configuration* (options-from-default-sources))
    (process-commandline-options
     :commandline     (list* program-name args)
     :version         (cl-rsb-tools-send-system:version/list :commit? t)
     :update-synopsis (curry #'update-synopsis :program-name program-name)
     :return          (lambda () (return-from main)))
    (enable-swank-on-signal))

  (unless (length= 2 (remainder))
    (error "~@<Supply event specification and destination URI.~@:>"))

  (let+ ((error-policy (maybe-relay-to-thread
                        (process-error-handling-options)))
         (method       (when-let ((value (getopt :long-name "method")))
                         (make-keyword value)))
         (meta-data    (iter (for value next (getopt :long-name "meta-data"))
                             (while value)
                             (appending (parse-meta-data value))))
         (timestamps   (iter (for value next (getopt :long-name "timestamp"))
                             (while value)
                             (appending (parse-timestamp value))))
         (causes       (iter (for value next (getopt :long-name "cause"))
                             (while value)
                             (collect (parse-cause value))))
         ((payload-spec &optional (destination "/")) (remainder)))
    (with-logged-warnings
      (with-error-policy (error-policy)
        ;; Load IDLs as specified on the commandline.
        (process-idl-options :purpose '(:packed-size :serializer :deserializer))

        (let ((command (make-command :send
                                     :destination  destination
                                     :method       method
                                     :meta-data    meta-data  ; TODO summarize these as some event-spec or something?
                                     :timestamps   timestamps
                                     :causes       causes
                                     :payload-spec payload-spec)))
          (with-interactive-interrupt-exit ()
            (command-execute command :error-policy error-policy)))))))

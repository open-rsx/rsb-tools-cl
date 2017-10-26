# CMakeLists.txt --- CMake build description file.
#
# Copyright (C) 2011-2017 Jan Moringen
#
# Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

cmake_minimum_required(VERSION 2.8.7)

# Extract version components from an approximately "git describe"-like
# formatted version string.
function(git_version_components INPUT OUT_MAJOR OUT_MINOR OUT_REVISION OUT_COMMIT)
    set(REGEX "^[-.a-z0-9]*([0-9]+)\\.([0-9]+)[-.]([0-9]+)-?(g[a-f0-9]+(:?-dirty)?|)$")
    string(REGEX REPLACE ${REGEX} "\\1" ${OUT_MAJOR}    "${INPUT}")
    string(REGEX REPLACE ${REGEX} "\\2" ${OUT_MINOR}    "${INPUT}")
    string(REGEX REPLACE ${REGEX} "\\3" ${OUT_REVISION} "${INPUT}")
    string(REGEX REPLACE ${REGEX} "\\4" ${OUT_COMMIT}   "${INPUT}")
    set(${OUT_MAJOR}    ${${OUT_MAJOR}}    PARENT_SCOPE)
    set(${OUT_MINOR}    ${${OUT_MINOR}}    PARENT_SCOPE)
    set(${OUT_REVISION} ${${OUT_REVISION}} PARENT_SCOPE)
    set(${OUT_COMMIT}   ${${OUT_COMMIT}}   PARENT_SCOPE)
endfunction()

# Git-based version: run git describe, parse the output and store the
# "revision" and "commit" components into version.sexp.
find_program(GIT_EXECUTABLE "git")
if(GIT_EXECUTABLE)
    function(git_based_version MAJOR MINOR)
        math(EXPR MINOR_MINUS_ONE "${MINOR} - 1")
        set(RELEASE_TAG "release-${MAJOR}.${MINOR_MINUS_ONE}")
        execute_process(COMMAND ${GIT_EXECUTABLE} describe
                                                  --long --dirty=-dirty
                                                  --tags --match "${RELEASE_TAG}"
                        RESULT_VARIABLE RESULT
                        OUTPUT_VARIABLE GIT_DESCRIBE_OUTPUT
                        OUTPUT_STRIP_TRAILING_WHITESPACE
                        ERROR_QUIET)
        if(RESULT EQUAL 0)
            git_version_components(${GIT_DESCRIBE_OUTPUT} IGN1 IGN2 REVISION COMMIT)
            file(WRITE "${CMAKE_SOURCE_DIR}/version.sexp" "${REVISION} \"${COMMIT}\"")
        else()
            message(STATUS "git describe failed (not a git repository?) in ${CMAKE_SOURCE_DIR}")
        endif()
    endfunction()
else()
    message(STATUS "git executable not found")
endif()

# Find and check SBCL installation.
set(SBCL_HOME $ENV{SBCL_HOME})
if(NOT SBCL_HOME)
    message(STATUS "SBCL_HOME is not set. Trying to find SBCL on default path.")
    find_program(SBCL_EXECUTABLE sbcl)
else()
    file(TO_CMAKE_PATH "${SBCL_HOME}" SBCL_HOME)
    message(STATUS "SBCL_HOME is set to ${SBCL_HOME}. Trying to find SBCL there.")
    find_program(SBCL_EXECUTABLE sbcl
                 PATHS "${SBCL_HOME}/../../bin" "${SBCL_HOME}"
                 NO_DEFAULT_PATH)
endif()
file(TO_CMAKE_PATH "${SBCL_EXECUTABLE}" SBCL_EXECUTABLE)
message(STATUS "SBCL executable: ${SBCL_EXECUTABLE}")
set(FASL_CACHE_DIR "${CMAKE_CURRENT_BINARY_DIR}/fasl-cache"
    CACHE FILEPATH
    "Directory in which the fasl cache should be created.")

# Configure Lisp environment.
set(LISP_RUNTIME_OPTIONS "" CACHE STRING
    "Runtime options to pass to the Lisp system.")
set(LISP_SOURCE_REGISTRY_ENTRIES "" CACHE STRING
    "Additional entries to place on ASDF source registry.")
set(LISP_FASL_CACHE_DIR "${CMAKE_CURRENT_BINARY_DIR}/fasl-cache"
    CACHE PATH
    "Directory in which the fasl cache should be created.")
set(LISP_INIT_FILE       "" CACHE FILEPATH
    "File to load as Lisp init file.")
set(LISP_INIT_CODE       "" CACHE STRING
    "Lisp code to be evaulated during initialization.")
set(LISP_INIT_BASE "")
if(LISP_INIT_FILE)
    set(LISP_INIT_BASE "${LISP_INIT_BASE} --userinit ${LISP_INIT_FILE}")
endif()
if(LISP_INIT_CODE)
    set(LISP_INIT_BASE "${LISP_INIT_BASE} --eval \"${LISP_INIT_CODE}\"")
endif()

set(LISP_LOAD)
function(add_silent_load SYSTEM)
    set(LISP_LOAD "${LISP_LOAD} --eval \"(let* ((*standard-output* (make-broadcast-stream)) (*trace-output* *standard-output*)) (ql:quickload :${SYSTEM}))\"" PARENT_SCOPE)
endfunction()

option(WITH_SSL_SUPPORT "Build with support for HTTPS in client and server." OFF)
if(NOT WITH_SSL_SUPPORT)
    set(LISP_INIT_BASE "--eval \"(progn (pushnew :drakma-no-ssl *features*) (pushnew :hunchentoot-no-ssl *features*))\" ${LISP_INIT_BASE}")
endif()

option(WITH_PROTOCOL_BUFFER_CONVERTER "Build with protocol buffer converter." ON)
if(WITH_PROTOCOL_BUFFER_CONVERTER)
    add_silent_load(rsb-converter-protocol-buffer)
endif()

option(WITH_REGEX_FILTER "Build with regex filter." ON)
if(WITH_REGEX_FILTER)
    add_silent_load(rsb-filter-regex)
endif()
option(WITH_XPATH_FILTER "Build with XPath filter." ON)
if(WITH_XPATH_FILTER)
    add_silent_load(rsb-filter-xpath)
endif()

option(WITH_INPROCESS_TRANSPORT "Build with inprocess transport." ON)
if(WITH_INPROCESS_TRANSPORT)
    add_silent_load(rsb-transport-inprocess)
endif()
option(WITH_SOCKET_TRANSPORT "Build with socket transport." ON)
if(WITH_SOCKET_TRANSPORT)
    add_silent_load(rsb-transport-socket)
endif()
option(WITH_SPREAD_TRANSPORT "Build with Spread transport." ON)
if(WITH_SPREAD_TRANSPORT)
    add_silent_load(rsb-transport-spread)
endif()
option(WITH_ROS_TRANSPORT "Build with ROS integration." ON)
if(WITH_ROS_TRANSPORT)
    add_silent_load(rsb-ros)
endif()
option(WITH_YARP_TRANSPORT "Build with YARP integreation." ON)
if(WITH_YARP_TRANSPORT)
    add_silent_load(rsb-yarp)
endif()

option(WITH_PNG_SUPPORT "Build with support for PNG image payloads." ON)
if(WITH_PNG_SUPPORT)
    add_silent_load(cl-rsb-formatting-png)
endif()
option(WITH_JSON_SUPPORT "Build with support for JSON formatting." ON)
if(WITH_JSON_SUPPORT)
    add_silent_load(rsb-formatting-json)
endif()
option(WITH_WEB_SUPPORT "Enable support for serving introspection and other information via HTTP." ON)
if(WITH_WEB_SUPPORT)
    add_silent_load(rsb-tools-commands-web)
endif()
option(WITH_WEB_RESOURCES_SUPPORT "Enable support for including web resources in the target binary." ON)
if(WITH_WEB_RESOURCES_SUPPORT)
    add_silent_load(rsb-tools-commands-web-resources)
endif()

# Execute Lisp processes, potentially with output redirection.
function(configure_lisp NAME CODE)
    set(NAME "${NAME}")
    set(DO   "${CODE} --quit")
    set(CL_SOURCE_REGISTRY       "(:source-registry
                                    (:tree \\\"${CMAKE_CURRENT_SOURCE_DIR}\\\")
                                    ${LISP_SOURCE_REGISTRY_ENTRIES}
                                    :inherit-configuration)")
    file(TO_CMAKE_PATH "${LISP_FASL_CACHE_DIR}" LISP_FASL_CACHE_DIR)
    set(ASDF_OUTPUT_TRANSLATIONS "(:output-translations
                                    (t (\\\"${LISP_FASL_CACHE_DIR}\\\" :implementation))
                                    :ignore-inherited-configuration)")
    configure_file("${CMAKE_CURRENT_SOURCE_DIR}/sbcl.cmake.in"
                   "${CMAKE_CURRENT_BINARY_DIR}/sbcl.${NAME}.cmake"
                   @ONLY)
    set(REDIRECTIONS)
endfunction()
function(configure_lisp_redirect NAME CODE)
    set(REDIRECTIONS "OUTPUT_FILE \"${CMAKE_CURRENT_BINARY_DIR}/${NAME}.out\"
                      ERROR_FILE  \"${CMAKE_CURRENT_BINARY_DIR}/${NAME}.err\"")
    configure_lisp(${NAME} ${CODE})
endfunction()
function(execute_lisp NAME CODE OUTPUT_VARIABLE)
    configure_lisp(${NAME} "${CODE}")
    message(STATUS "Executing Lisp process \"${NAME}\"")
    execute_process(COMMAND cmake -P "${CMAKE_CURRENT_BINARY_DIR}/sbcl.${NAME}.cmake"
                    OUTPUT_VARIABLE ${OUTPUT_VARIABLE}
                    OUTPUT_STRIP_TRAILING_WHITESPACE)
    set(${OUTPUT_VARIABLE} ${${OUTPUT_VARIABLE}} PARENT_SCOPE)
endfunction()

# Project definition (version is grovelled from asd system definition,
# possibly in combination with version.sexp file written below).
# C compiler is needed for CLON's stream-width determination feature
set(SYSTEM_NAME "cl-rsb-tools-logger")
project(rsb-tools "C")

# Determine at least major and minor version components.
set(LISP_INIT "${LISP_INIT_BASE}")
set(REDIRECTIONS "ERROR_FILE \"${CMAKE_CURRENT_BINARY_DIR}/version.1.err\"")
execute_lisp("version"
             "--eval \"(asdf:find-system :${SYSTEM_NAME})\"
              --eval \"(princ (${SYSTEM_NAME}-system:version/string :commit? t))\""
             VERSION_FROM_LISP)
git_version_components(${VERSION_FROM_LISP} RSB_TOOLS_VERSION_MAJOR
                                            RSB_TOOLS_VERSION_MINOR
                                            RSB_TOOLS_VERSION_PATCH
                                            RSB_TOOLS_VERSION_COMMIT)

# If necessary, try to run git describe and determine remaining
# version components.
if(GIT_EXECUTABLE AND (NOT RSB_TOOLS_VERSION_PATCH))
    git_based_version(${RSB_TOOLS_VERSION_MAJOR} ${RSB_TOOLS_VERSION_MINOR})
    set(REDIRECTIONS "ERROR_FILE \"${CMAKE_CURRENT_BINARY_DIR}/version.2.err\"")
    execute_lisp("version"
                 "--eval \"(asdf:find-system :${SYSTEM_NAME})\"
                  --eval \"(princ (${SYSTEM_NAME}-system:version/string :commit? t))\""
                 VERSION_FROM_LISP)
    git_version_components(${VERSION_FROM_LISP} RSB_TOOLS_VERSION_MAJOR
                                                RSB_TOOLS_VERSION_MINOR
                                                RSB_TOOLS_VERSION_PATCH
                                                RSB_TOOLS_VERSION_COMMIT)
endif()
set(VERSION_SUFFIX "${RSB_TOOLS_VERSION_MAJOR}.${RSB_TOOLS_VERSION_MINOR}"
    CACHE STRING
    "Suffix to append to installed binaries and share/ sub-directory.")

message(STATUS "Project is ${CMAKE_PROJECT_NAME} (${VERSION_FROM_LISP})")

# Create binary.
set(MAIN_NAME        "tools")
set(MAIN_SYSTEM_NAME "cl-rsb-tools-main")
set(MAIN_EXECUTABLE  "${CMAKE_CURRENT_BINARY_DIR}/${MAIN_NAME}${CMAKE_EXECUTABLE_SUFFIX}")
set(TOOLS            info logger call send introspect web bridge server)
set(LISP_INIT "${LISP_INIT_BASE} ${LISP_LOAD}")
configure_lisp_redirect("${MAIN_NAME}"
                        "--eval \"(load-system :iterate)\"
                         --eval \"(setf iterate::*always-declare-variables* t)\"
                         --eval \"(asdf:operate (quote asdf:program-op) :${MAIN_SYSTEM_NAME})\"")
add_custom_command(OUTPUT  "${MAIN_EXECUTABLE}"
                   COMMAND "${CMAKE_COMMAND}" -P "${CMAKE_CURRENT_BINARY_DIR}/sbcl.${MAIN_NAME}.cmake"
                   COMMENT "Creating Lisp image ${MAIN_NAME} (this can take a long time)")
add_custom_target(binary ALL DEPENDS "${MAIN_EXECUTABLE}")

# Download third-party libraries for web command
if(WITH_WEB_SUPPORT)
    add_subdirectory(resources/lib)
endif()

# Tests
enable_testing()

# Lift-based tests for formatting and stats systems.
foreach(SYSTEM rsb-tools-common cl-rsb-formatting cl-rsb-stats
               rsb-tools-commands rsb-tools-commands-web)
    set(LISP_INIT "${LISP_INIT_BASE}")
    configure_lisp_redirect("test-${SYSTEM}"
                            "--eval \"(proclaim (quote (optimize (debug 3) (safety 3) (speed 0))))\"
                             --eval \"(asdf:test-system :${SYSTEM})\"")
    add_test(NAME    "test-${SYSTEM}"
             COMMAND "${CMAKE_COMMAND}" -P "${CMAKE_CURRENT_BINARY_DIR}/sbcl.test-${SYSTEM}.cmake")
    set_tests_properties("test-${SYSTEM}" PROPERTIES RUN_SERIAL TRUE)
endforeach()

set(SEND_EVENT_AFTER_TWO_SECONDS "(let ((timer (sb-ext:make-timer (lambda () (rsb:with-participant (i :informer \"inprocess:\") (rsb:send i 1)))))) (sb-ext:schedule-timer timer 2))")
set(EXIT_AFTER_THREE_SECONDS "(let ((timer (sb-ext:make-timer (lambda () (sb-ext:exit))))) (sb-ext:schedule-timer timer 3))")

# --help, --help-all and --version for all binaries.
foreach(TOOL ${TOOLS})
    add_test(NAME    "${TOOL}/help"
             COMMAND "${MAIN_EXECUTABLE}" "${TOOL}" --help)
    add_test(NAME    "${TOOL}/help-all"
             COMMAND "${MAIN_EXECUTABLE}" "${TOOL}" --help-for=all)
    add_test(NAME    "${TOOL}/version"
             COMMAND "${MAIN_EXECUTABLE}" "${TOOL}" --version)
endforeach()

# All categories for info binary.
foreach(CATEGORY verbose configuration transports converters filters
                 transforms event-processing participants)
    add_test(NAME    "info/category=${CATEGORY}"
             COMMAND "${MAIN_EXECUTABLE}" info --debug --${CATEGORY})
endforeach()

# All error policies for logger, send, call and bridge binaries.
foreach(POLICY continue abort)
    add_test(NAME    "logger/on-error=${POLICY}"
            COMMAND  "${MAIN_EXECUTABLE}" logger
                     --debug --on-error "${POLICY}"
                     --eval "${EXIT_AFTER_THREE_SECONDS}"
                     inprocess:)
    add_test(NAME    "send/on-error=${POLICY}"
             COMMAND "${MAIN_EXECUTABLE}" send
                     --debug --on-error "${POLICY}"
                     1 inprocess:)
    add_test(NAME    "call/on-error=${POLICY}"
             COMMAND "${MAIN_EXECUTABLE}" call
                     --debug --on-error "${POLICY}"
                     --no-wait
                     "inprocess:/foo/bar(1)")
    add_test(NAME    "bridge/on-error=${POLICY}"
            COMMAND  "${MAIN_EXECUTABLE}" bridge
                     --debug --on-error "${POLICY}"
                     --eval "${EXIT_AFTER_THREE_SECONDS}"
                     "inprocess:/foo -> inprocess:/bar")
endforeach()

# IDL loading and error policies for logger, send, call and bridge
# binaries.
function(idl_tests SUFFIX OPTIONS)
    add_test(NAME   "logger/${SUFFIX}"
             COMMAND "${MAIN_EXECUTABLE}" logger
                     --debug
                     ${OPTIONS}
                     --eval "${EXIT_AFTER_THREE_SECONDS}"
                     inprocess:)
    add_test(NAME   "send/${SUFFIX}"
             COMMAND "${MAIN_EXECUTABLE}" send
                     --debug
                     ${OPTIONS}
                     1 inprocess:)
    add_test(NAME   "call/${SUFFIX}"
             COMMAND "${MAIN_EXECUTABLE}" call
                     --debug
                     ${OPTIONS}
                     --no-wait
                     "inprocess:/foo/bar(1)")
    add_test(NAME   "bridge/${SUFFIX}"
             COMMAND "${MAIN_EXECUTABLE}" bridge
                     --debug
                     ${OPTIONS}
                     --eval "${EXIT_AFTER_THREE_SECONDS}"
                     "inprocess:/foo -> inprocess:/bar")
endfunction()

idl_tests(idl-path/on-error=contiue "--on-error=continue;--idl-path;no-such-directory")
idl_tests(load-idl/simple           "--load-idl=${CMAKE_SOURCE_DIR}/test/data/test/Simple.proto")
idl_tests(load-idl/crlf             "--load-idl=${CMAKE_SOURCE_DIR}/test/data/test/CRLF.proto")
idl_tests(on-demand-idl-loading     "--idl-path=${CMAKE_SOURCE_DIR}/test/data/;--on-demand-idl-loading=blocking")

add_test(NAME   "send/protocol-buffer-payload"
         COMMAND "${MAIN_EXECUTABLE}" send
         --debug
         --on-demand-idl-loading=blocking --idl-path=${CMAKE_SOURCE_DIR}/test/data/
         "pb:.test.Simple:{data:\"foo\"}" inprocess:)
add_test(NAME   "call/protocol-buffer-payload"
         COMMAND "${MAIN_EXECUTABLE}" call
                 --debug
                 --on-demand-idl-loading=blocking --idl-path=${CMAKE_SOURCE_DIR}/test/data/
                 --no-wait
                 "inprocess:/foo/bar(pb:.test.Simple:{data:\"foo\"})")

# Event meta-data specifications for send binary.
foreach(ARG "--method=foo"
            "--meta-data=foo=bar"
            "--timestamp=foo=2013-03-27T01:32:43.158404+01:00"
            "--timestamp=foo=2013-03-27T01:32:43.158404"
            "--timestamp=foo=2013-03-27T01:32:43"
            "--timestamp=foo=2013-03-27"
            "--cause=12345678-1234-1234-1234-567899999999:1")
    string(REGEX REPLACE "[- ]" "" CLEAN_ARG "${ARG}")
    add_test(NAME   "send/meta-data/${CLEAN_ARG}"
             COMMAND "${MAIN_EXECUTABLE}" send
                     --debug
                     ${ARG} 1 inprocess:)
endforeach()

# Payload specifications for call and send binaries.
foreach(PAYLOAD ""
                "true"
                "false"
                "\"string\""
                "1"
                "1.5"
                "/scope/"
                "#P\"${CMAKE_CURRENT_LIST_FILE}\""
                "#P\"${CMAKE_CURRENT_LIST_FILE}\":utf-8"
                "#P\"${CMAKE_CURRENT_LIST_FILE}\":binary"
                "pb:.rsb.protocol.Notification:{}"
                "pb:.rsb.protocol.Notification:{data:\"\\000\"}"
                "pb:.rsb.protocol.Notification:{event_id:{sequence_number:1}}")
    string(REGEX REPLACE "[- #]" "" CLEAN_PAYLOAD "${PAYLOAD}")
    add_test(NAME   "send/payload/${CLEAN_PAYLOAD}"
             COMMAND "${MAIN_EXECUTABLE}" send
                     --debug
                     "${PAYLOAD}" inprocess:)
    add_test(NAME   "call/payload/${CLEAN_PAYLOAD}"
             COMMAND "${MAIN_EXECUTABLE}" call
                     --debug
                     --no-wait
                     "inprocess:/foo/bar(${PAYLOAD})")
endforeach()

# Multiple URIs, queue capacity and stop-after for logger
add_test(NAME    "logger/multiple-uris"
         COMMAND "${MAIN_EXECUTABLE}" logger
                 --debug
                 --eval "${EXIT_AFTER_THREE_SECONDS}"
                 inprocess:/foo inprocess:/bar)
add_test(NAME    "logger/queue-capacity"
         COMMAND "${MAIN_EXECUTABLE}" logger
                 --debug
                 --eval "${EXIT_AFTER_THREE_SECONDS}"
                 --max-queued-events 1
                 inprocess:/foo)
add_test(NAME    "logger/stop-after"
         COMMAND "${MAIN_EXECUTABLE}" logger
                 --debug
                 --eval "${EXIT_AFTER_THREE_SECONDS}"
                 --stop-after 10
                 inprocess:/foo)

# Filters for logger
add_test(NAME    "logger/filter"
         COMMAND "${MAIN_EXECUTABLE}" logger
                 --debug
                 --eval "${EXIT_AFTER_THREE_SECONDS}"
                 --filter "scope :scope \"/foo\""
                 inprocess:/)

# All styles for logger and call binaries.
# TODO: add these:
# programmable/template :template #p"${CMAKE_CURRENT_SOURCE_DIR}/test/data/empty.template"
# programmable/script   :template #p"${CMAKE_CURRENT_SOURCE_DIR}/test/data/empty.lisp"
# compact :columns (:now (:scope :width 12))
foreach(STYLE discard meta-data payload detailed
              columns compact statistics
              monitor monitor/scope monitor/origin monitor/type monitor/size
              timeline/scope timeline/origin
              json)
    add_test(NAME    "logger/style=${STYLE}"
             COMMAND "${MAIN_EXECUTABLE}" logger
                     --debug --style "${STYLE}"
                     --eval "${SEND_EVENT_AFTER_TWO_SECONDS}"
                     --eval "${EXIT_AFTER_THREE_SECONDS}"
                     inprocess:)
    add_test(NAME   "call/style=${STYLE}/no-wait"
            COMMAND "${MAIN_EXECUTABLE}" call
                    --debug --style "${STYLE}"
                    --no-wait
                    "inprocess:/foo/bar(1)")
    add_test(NAME   "call/style=${STYLE}/wait"
            COMMAND "${MAIN_EXECUTABLE}" call
                    --debug --style "${STYLE}"
                    --eval "(progn (bt:make-thread (lambda () (rsb:with-participant (s :local-server \"inprocess:/foo\") (rsb.patterns.request-reply:with-methods (s) ((\"bar\" (x integer) x)) (sleep 2))))) (sleep 1))"
                    "inprocess:/foo/bar(1)")
endforeach()

# Introspection tests
foreach(URLS "inprocess:" "inprocess:;inprocess:/")
  foreach(STYLE "" "--style;object-tree" "--style;monitor/object-tree"
                "--style;monitor/events" "--style;json")
    string(REGEX REPLACE "[;:]" "_" CLEAN_URLS "${URLS}")
    string(REGEX REPLACE "[;-]" "_" CLEAN_STYLE "${STYLE}")

    set(EXIT)
    if (STYLE MATCHES "monitor/object-tree|monitor/events")
      set(EXIT "--eval;${EXIT_AFTER_THREE_SECONDS}")
    endif()

    add_test(NAME    "introspect/${CLEAN_URLS}/${CLEAN_STYLE}"
             COMMAND "${MAIN_EXECUTABLE}" introspect
                     --debug
                     ${EXIT} ${STYLE} ${URLS})
  endforeach()
endforeach()

# Bridge tests
add_test(NAME    "bridge/smoke/unidirectional"
         COMMAND "${MAIN_EXECUTABLE}" bridge
                 --debug
                 --eval "${EXIT_AFTER_THREE_SECONDS}"
                 "inprocess:/foo -> inprocess:/bar")
add_test(NAME    "bridge/smoke/transform"
         COMMAND "${MAIN_EXECUTABLE}" bridge
                 --debug
                 --eval "${EXIT_AFTER_THREE_SECONDS}"
                 "inprocess:/foo -> /prefix-scope :prefix \"/baz\"/ inprocess:/bar")
add_test(NAME    "bridge/smoke/bidirectional"
         COMMAND "${MAIN_EXECUTABLE}" bridge
                 --debug
                 --eval "${EXIT_AFTER_THREE_SECONDS}"
                 "inprocess:/foo <-> inprocess:/bar")

# Server test
add_test(NAME    "server/tcp/smoke"
         COMMAND "${MAIN_EXECUTABLE}" server
                 --debug
                 --eval "${EXIT_AFTER_THREE_SECONDS}"
                 "socket://localhost:0?portfile=-")
add_test(NAME    "server/tcp/two-ports"
         COMMAND "${MAIN_EXECUTABLE}" server
                 --debug
                 --eval "${EXIT_AFTER_THREE_SECONDS}"
                 "socket://localhost:0?portfile=-" "tcp-socket://localhost:0?portfile=-")
add_test(NAME    "server/unix/smoke"
         COMMAND "${MAIN_EXECUTABLE}" server
                 --debug
                 --eval "${EXIT_AFTER_THREE_SECONDS}"
                 "unix:?name=socket")

# redump test
add_test("redump/simple"
         sh -c "cp \"${MAIN_EXECUTABLE}\" ./temp && ./temp redump")
add_test(NAME    "redump/name"
         COMMAND "${MAIN_EXECUTABLE}" redump redumped)
add_test(NAME    "redump/compress"
         COMMAND "${MAIN_EXECUTABLE}" redump redumped-compress compress)
add_test(NAME    "redump/static"
         COMMAND "${MAIN_EXECUTABLE}" redump redumped-static static)

# Install binary
install(CODE "message(STATUS \"Configuring Lisp image ${MAIN_NAME} for installation\")
              execute_process(COMMAND \"${MAIN_EXECUTABLE}\" redump)")
set(BINARY_PREFIX    "rsb-")
set(BINARY_SUFFIX    "cl${VERSION_SUFFIX}")
set(MAIN_BINARY_NAME "${BINARY_PREFIX}${MAIN_NAME}${BINARY_SUFFIX}${CMAKE_EXECUTABLE_SUFFIX}"
    CACHE STRING
    "Name of the installed binary.")
install(PROGRAMS    "${MAIN_EXECUTABLE}"
        DESTINATION "bin"
        RENAME      "${MAIN_BINARY_NAME}")

# Install scripts
install(DIRECTORY   "scripts"
        DESTINATION "share/${CMAKE_PROJECT_NAME}${BINARY_SUFFIX}/")

# Install source
set(SOURCE_INSTALL_DIR "share/common-lisp/source/${CMAKE_PROJECT_NAME}${VERSION_SUFFIX}")

file(GLOB ASD_FILES
     RELATIVE "${CMAKE_CURRENT_SOURCE_DIR}"
     "*.asd")
file(GLOB SEXP_FILES
     RELATIVE "${CMAKE_CURRENT_SOURCE_DIR}"
     "*.sexp")

foreach(directory src common formatting stats)
    install(DIRECTORY      "${CMAKE_CURRENT_SOURCE_DIR}/${directory}"
            DESTINATION    "${SOURCE_INSTALL_DIR}"
            FILES_MATCHING PATTERN "*.lisp")
endforeach()
install(DIRECTORY      "${CMAKE_CURRENT_SOURCE_DIR}/test"
        DESTINATION    "${SOURCE_INSTALL_DIR}"
        FILES_MATCHING PATTERN "*.lisp")
install(DIRECTORY
        DESTINATION "${SOURCE_INSTALL_DIR}")
install(FILES       ${ASD_FILES} ${SEXP_FILES}
        DESTINATION "${SOURCE_INSTALL_DIR}")

# Packaging
set(PACKAGE_BASE_NAME           "${CMAKE_PROJECT_NAME}-${BINARY_SUFFIX}")
set(CPACK_PACKAGE_VERSION_MAJOR ${RSB_TOOLS_VERSION_MAJOR})
set(CPACK_PACKAGE_VERSION_MINOR ${RSB_TOOLS_VERSION_MINOR})
set(CPACK_PACKAGE_VERSION_PATCH ${RSB_TOOLS_VERSION_PATCH})
set(CPACK_PACKAGE_VERSION       "${CPACK_PACKAGE_VERSION_MAJOR}.${CPACK_PACKAGE_VERSION_MINOR}.${CPACK_PACKAGE_VERSION_PATCH}")
set(CPACK_PACKAGE_VENDOR        "CoR-Lab, Bielefeld University")
set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_SOURCE_DIR}/COPYING")

include("${CMAKE_SOURCE_DIR}/cpack/CPackInclude.cmake")

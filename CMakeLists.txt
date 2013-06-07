# CMakeLists.txt --- CMake build description file.
#
# Copyright (C) 2011, 2012, 2013 Jan Moringen
#
# Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
#
# This file may be licensed under the terms of of the GNU Lesser
# General Public License Version 3 (the ``LGPL''), or (at your option)
# any later version.
#
# Software distributed under the License is distributed on an ``AS
# IS'' basis, WITHOUT WARRANTY OF ANY KIND, either express or
# implied. See the LGPL for the specific language governing rights and
# limitations.
#
# You should have received a copy of the LGPL along with this
# program. If not, go to http://www.gnu.org/licenses/lgpl.html or
# write to the Free Software Foundation, Inc., 51 Franklin Street,
# Fifth Floor, Boston, MA 02110-1301, USA.
#
# The development of this software was supported by:
#   CoR-Lab, Research Institute for Cognition and Robotics
#     Bielefeld University

cmake_minimum_required(VERSION 2.8)

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
                                                  --match "${RELEASE_TAG}"
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
    message(STATUS "SBCL_HOME is set to ${SBCL_HOME}. Trying to find SBCL there.")
    find_program(SBCL_EXECUTABLE sbcl
                 PATHS "${SBCL_HOME}/../../bin" ${SBCL_HOME}
                 NO_DEFAULT_PATH)
endif()
message(STATUS "SBCL executable: ${SBCL_EXECUTABLE}")
set(FASL_CACHE_DIR "${CMAKE_CURRENT_BINARY_DIR}/fasl-cache"
    CACHE FILEPATH
    "Directory in which the fasl cache should be created.")

# Configure Lisp environment.
set(LISP_RUNTIME_OPTIONS "" CACHE STRING
    "Runtime options to pass to the Lisp system.")
set(LISP_INIT_FILE       "" CACHE FILEPATH
    "File to load as Lisp init file.")
set(LISP_INIT_CODE       "" CACHE STRING
    "Lisp code to be evaulated during initialization.")
set(LISP_INIT "")
if(LISP_INIT_FILE)
    set(LISP_INIT "${LISP_INIT} --userinit ${LISP_INIT_FILE}")
endif()
if(LISP_INIT_CODE)
    set(LISP_INIT "${LISP_INIT} --eval \"${LISP_INIT_CODE}\"")
endif()

set(CL_SOURCE_REGISTRY       "(:source-registry (:tree \\\"${CMAKE_CURRENT_SOURCE_DIR}\\\") :inherit-configuration)")
set(ASDF_OUTPUT_TRANSLATIONS "(:output-translations (t (\\\"${FASL_CACHE_DIR}\\\" :implementation)) :ignore-inherited-configuration)")

# Execute Lisp processes, potentially with output redirection.
function(configure_lisp NAME CODE)
    set(NAME "${NAME}")
    set(DO   "${CODE} --quit")
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
set(SYSTEM_NAME "cl-rsb-tools-logger")
project(rsb-tools)

# Determine at least major and minor version components.
set(REDIRECTIONS "ERROR_QUIET")
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
if(NOT RSB_TOOLS_VERSION_PATCH)
    git_based_version(${RSB_TOOLS_VERSION_MAJOR} ${RSB_TOOLS_VERSION_MINOR})
    set(REDIRECTIONS "ERROR_QUIET")
    execute_lisp("version"
                 "--eval \"(asdf:find-system :${SYSTEM_NAME})\"
                  --eval \"(princ (${SYSTEM_NAME}-system:version/string :commit? t))\""
                 VERSION_FROM_LISP)
    git_version_components(${VERSION_FROM_LISP} RSB_TOOLS_VERSION_MAJOR
                                                RSB_TOOLS_VERSION_MINOR
                                                RSB_TOOLS_VERSION_PATCH
                                                RSB_TOOLS_VERSION_COMMIT)
endif()

message(STATUS "Project is ${CMAKE_PROJECT_NAME} (${VERSION_FROM_LISP})")

# Create binary.
set(MAIN_NAME "tools")
set(TOOLS info logger call send)
configure_lisp_redirect("${MAIN_NAME}"
                        "--load \"${CMAKE_CURRENT_SOURCE_DIR}/main/dump.lisp\"")
add_custom_command(OUTPUT  "${CMAKE_CURRENT_BINARY_DIR}/${MAIN_NAME}"
                   COMMAND "${CMAKE_COMMAND}" -P "${CMAKE_CURRENT_BINARY_DIR}/sbcl.${MAIN_NAME}.cmake"
                   COMMENT "Creating Lisp image ${MAIN_NAME} (this can take a long time)")
add_custom_target(binary ALL DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/${MAIN_NAME}")

# Create symlinks.
if(NOT CMAKE_SYSTEM_NAME STREQUAL "Windows")
    message(STATUS "Configuring tool ${TOOLS}")
    add_custom_command(OUTPUT  "${CMAKE_CURRENT_BINARY_DIR}/info"
                       COMMAND "${CMAKE_CURRENT_BINARY_DIR}/${MAIN_NAME}" create-links
                       DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/${MAIN_NAME}"
                       COMMENT "Creating symlinks")
    add_custom_target(links ALL DEPENDS info)
endif()

# Tests
enable_testing()

# Lift-based tests for formatting and stats systems.
foreach(SYSTEM common formatting stats)
    configure_lisp_redirect("test-${SYSTEM}"
                            "--eval \"(proclaim (quote (optimize (debug 3) (safety 3) (speed 0))))\"
                             --eval \"(asdf:test-system :cl-rsb-${SYSTEM})\"")
    add_test(NAME    "test-${SYSTEM}"
             COMMAND "${CMAKE_COMMAND}" -P "${CMAKE_CURRENT_BINARY_DIR}/sbcl.test-${SYSTEM}.cmake")
    set_tests_properties("test-${SYSTEM}" PROPERTIES RUN_SERIAL TRUE)
endforeach()

# --help, --help-all and --version for all binaries.
foreach(TOOL ${TOOLS})
    add_test(NAME    "${TOOL}/help"
             COMMAND "${CMAKE_CURRENT_BINARY_DIR}/${TOOL}" --help)
    add_test(NAME    "${TOOL}/help-all"
             COMMAND "${CMAKE_CURRENT_BINARY_DIR}/${TOOL}" --help-for=all)
    add_test(NAME    "${TOOL}/version"
             COMMAND "${CMAKE_CURRENT_BINARY_DIR}/${TOOL}" --version)
endforeach()

# All categories for info binary.
foreach(CATEGORY verbose configuration connectors converters filters event-processing)
    add_test(NAME    "info/category=${CATEGORY}"
             COMMAND "${CMAKE_CURRENT_BINARY_DIR}/info" --debug --${CATEGORY})
endforeach()

# All error policies for logger, send and call binaries.
foreach(POLICY continue abort)
    add_test(NAME   "logger/on-error=${POLICY}"
            COMMAND "${CMAKE_CURRENT_BINARY_DIR}/logger"
                    --debug --on-error "${POLICY}"
                    --eval "(let ((timer (sb-ext:make-timer (lambda () (sb-ext:exit))))) (sb-ext:schedule-timer timer 3))"
                     inprocess:)
    foreach(ARG ""
                "--method=foo"
                "--meta-data=foo=bar"
                "--timestamp=foo=2013-03-27T01:32:43.158404+01:00"
                "--timestamp=foo=2013-03-27T01:32:43.158404"
                "--timestamp=foo=2013-03-27T01:32:43"
                "--timestamp=foo=2013-03-27"
                "--cause=12345678-1234-1234-1234-567899999999:1")
        string(REGEX REPLACE "[- ]" "" CLEAN_ARG "${ARG}")
        add_test(NAME   "send/on-error=${POLICY}/${CLEAN_ARG}"
                 COMMAND "${CMAKE_CURRENT_BINARY_DIR}/send"
                         --debug --on-error "${POLICY}"
                         ${ARG} 1 inprocess:)
    endforeach()
    add_test(NAME   "call/on-error=${POLICY}"
            COMMAND "${CMAKE_CURRENT_BINARY_DIR}/call"
                    --debug --on-error "${POLICY}"
                    --no-wait
                    "inprocess:/foo/bar(1)")
endforeach()

# All styles for logger and call binaries.
# TODO: add these:
# programmable/template :template #p"${CMAKE_CURRENT_SOURCE_DIR}/test/data/empty.template"
# programmable/script   :template #p"${CMAKE_CURRENT_SOURCE_DIR}/test/data/empty.lisp"
foreach(STYLE discard meta-data payload detailed
              compact statistics
              monitor monitor/scope monitor/origin monitor/type monitor/size
              timeline/scope timeline/origin)
    add_test(NAME    "logger/style=${STYLE}"
             COMMAND "${CMAKE_CURRENT_BINARY_DIR}/logger"
                     --debug --style "${STYLE}"
                     --eval "(let ((timer (sb-ext:make-timer (lambda () (sb-ext:exit))))) (sb-ext:schedule-timer timer 3))"
                     inprocess:)
    add_test(NAME   "call/style=${STYLE}/no-wait"
            COMMAND "${CMAKE_CURRENT_BINARY_DIR}/call"
                    --debug --style "${STYLE}"
                    --no-wait
                    "inprocess:/foo/bar(1)")
    add_test(NAME   "call/style=${STYLE}/wait"
            COMMAND "${CMAKE_CURRENT_BINARY_DIR}/call"
                    --debug --style "${STYLE}"
                    --eval "(progn (bt:make-thread (lambda () (rsb.patterns:with-local-server (s \"inprocess:/foo\") (rsb.patterns:with-methods (s) ((\"bar\" (x integer) x)) (sleep 2))))) (sleep 1))"
                    "inprocess:/foo/bar(1)")
endforeach()

# Installation
set(BINARY_PREFIX    "rsb-")
set(VERSION_SUFFIX   "cl${RSB_TOOLS_VERSION_MAJOR}.${RSB_TOOLS_VERSION_MINOR}")
set(MAIN_BINARY_NAME ${BINARY_PREFIX}${MAIN_NAME}${VERSION_SUFFIX})
install(PROGRAMS    "${CMAKE_CURRENT_BINARY_DIR}/${MAIN_NAME}"
        DESTINATION "bin"
        RENAME      "${MAIN_BINARY_NAME}")

# Packaging
set(PACKAGE_BASE_NAME           "${CMAKE_PROJECT_NAME}-${VERSION_SUFFIX}")
set(CPACK_PACKAGE_VERSION_MAJOR ${RSB_TOOLS_VERSION_MAJOR})
set(CPACK_PACKAGE_VERSION_MINOR ${RSB_TOOLS_VERSION_MINOR})
set(CPACK_PACKAGE_VERSION_PATCH ${RSB_TOOLS_VERSION_PATCH})
set(CPACK_PACKAGE_VERSION       "${CPACK_PACKAGE_VERSION_MAJOR}.${CPACK_PACKAGE_VERSION_MINOR}.${CPACK_PACKAGE_VERSION_PATCH}")
set(CPACK_PACKAGE_VENDOR        "CoR-Lab, Bielefeld University")
set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_SOURCE_DIR}/COPYING")

include("${CMAKE_SOURCE_DIR}/cpack/CPackInclude.cmake")

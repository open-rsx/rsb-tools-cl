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

CMAKE_MINIMUM_REQUIRED(VERSION 2.8)

# Extract version components from an approximately "git describe"-like
# formatted version string.
MACRO(GIT_VERSION_COMPONENTS INPUT OUT_MAJOR OUT_MINOR OUT_REVISION OUT_COMMIT)
    SET(REGEX "^[-.a-z0-9]*([0-9]+)\\.([0-9]+)[-.]([0-9]+)-?(g[a-f0-9]+(:?-dirty)?|)$")
    STRING(REGEX REPLACE ${REGEX} "\\1" ${OUT_MAJOR}    "${INPUT}")
    STRING(REGEX REPLACE ${REGEX} "\\2" ${OUT_MINOR}    "${INPUT}")
    STRING(REGEX REPLACE ${REGEX} "\\3" ${OUT_REVISION} "${INPUT}")
    STRING(REGEX REPLACE ${REGEX} "\\4" ${OUT_COMMIT}   "${INPUT}")
ENDMACRO()

# Git-based version.
FIND_PROGRAM(GIT_EXECUTABLE "git")
IF(GIT_EXECUTABLE)
    EXECUTE_PROCESS(COMMAND ${GIT_EXECUTABLE} describe --long --dirty=-dirty
                    RESULT_VARIABLE RESULT
                    OUTPUT_VARIABLE GIT_DESCRIBE_OUTPUT
                    OUTPUT_STRIP_TRAILING_WHITESPACE
                    ERROR_QUIET)
    IF(RESULT EQUAL 0)
        GIT_VERSION_COMPONENTS(${GIT_DESCRIBE_OUTPUT} IGN1 IGN2 REVISION COMMIT)
        FILE(WRITE "${CMAKE_SOURCE_DIR}/version.sexp" "${REVISION} \"${COMMIT}\"")
    ELSE()
        MESSAGE(STATUS "Not a git repository: ${CMAKE_SOURCE_DIR}")
    ENDIF()
ELSE()
    MESSAGE(STATUS "git executable not found")
ENDIF()

# Find and check SBCL installation.
SET(SBCL_HOME $ENV{SBCL_HOME})
IF(NOT SBCL_HOME)
    MESSAGE(STATUS "SBCL_HOME is not set. Trying to find SBCL on default path.")
    FIND_PROGRAM(SBCL_EXECUTABLE sbcl)
ELSE()
    MESSAGE(STATUS "SBCL_HOME is set to ${SBCL_HOME}. Trying to find SBCL there.")
    FIND_PROGRAM(SBCL_EXECUTABLE sbcl
                 PATHS "${SBCL_HOME}/../../bin" ${SBCL_HOME}
                 NO_DEFAULT_PATH)
ENDIF()
MESSAGE(STATUS "SBCL executable: ${SBCL_EXECUTABLE}")

# Configure Lisp environment.
SET(LISP_RUNTIME_OPTIONS "" CACHE STRING
    "Runtime options to pass to the Lisp system.")
SET(LISP_INIT_FILE       "" CACHE FILEPATH
    "File to load as Lisp init file.")
SET(LISP_INIT_CODE       "" CACHE STRING
    "Lisp code to be evaulated during initialization.")
SET(LISP_INIT "")
IF(LISP_INIT_FILE)
    SET(LISP_INIT "${LISP_INIT} --userinit ${LISP_INIT_FILE}")
ENDIF()
IF(LISP_INIT_CODE)
    SET(LISP_INIT "${LISP_INIT} --eval \"${LISP_INIT_CODE}\"")
ENDIF()

SET(CL_SOURCE_REGISTRY       "(:source-registry (:tree \\\"${CMAKE_CURRENT_SOURCE_DIR}\\\") :inherit-configuration)")
SET(ASDF_OUTPUT_TRANSLATIONS "(:output-translations (t (\\\"${CMAKE_CURRENT_BINARY_DIR}/fasl-cache\\\" :implementation)) :ignore-inherited-configuration)")

# Execute Lisp process
MACRO(CONFIGURE_LISP NAME CODE)
    SET(NAME "${NAME}")
    SET(DO   "${CODE} --quit")
    CONFIGURE_FILE("${CMAKE_CURRENT_SOURCE_DIR}/sbcl.cmake.in"
                   "${CMAKE_CURRENT_BINARY_DIR}/sbcl.${NAME}.cmake"
                   @ONLY)
    SET(REDIRECTIONS)
ENDMACRO()
MACRO(CONFIGURE_LISP_REDIRECT NAME CODE)
    SET(REDIRECTIONS "OUTPUT_FILE \"${CMAKE_CURRENT_BINARY_DIR}/${NAME}.out\"
                      ERROR_FILE  \"${CMAKE_CURRENT_BINARY_DIR}/${NAME}.err\"")
    CONFIGURE_LISP(${NAME} ${CODE})
ENDMACRO()
MACRO(EXECUTE_LISP NAME CODE OUTPUT_VARIABLE)
    CONFIGURE_LISP(${NAME} "${CODE}")
    MESSAGE(STATUS "Executing Lisp process \"${NAME}\"")
    EXECUTE_PROCESS(COMMAND cmake -P "${CMAKE_CURRENT_BINARY_DIR}/sbcl.${NAME}.cmake"
                    OUTPUT_VARIABLE ${OUTPUT_VARIABLE}
                    OUTPUT_STRIP_TRAILING_WHITESPACE)
ENDMACRO()

# Project definition (version is grovelled from asd system definition
# and, possibly in combination with version.sexp file written above).
SET(SYSTEM_NAME "cl-rsb-tools-logger")
PROJECT(rsb-tools)
SET(REDIRECTIONS "ERROR_QUIET")
EXECUTE_LISP("version"
             "--eval \"(asdf:find-system :${SYSTEM_NAME})\"
              --eval \"(princ (${SYSTEM_NAME}-system:version/string :commit? t))\""
             VERSION_FROM_LISP)
GIT_VERSION_COMPONENTS(${VERSION_FROM_LISP} RSB_TOOLS_VERSION_MAJOR
                                            RSB_TOOLS_VERSION_MINOR
                                            RSB_TOOLS_VERSION_PATCH
                                            RSB_TOOLS_VERSION_COMMIT)
MESSAGE(STATUS "Project is ${CMAKE_PROJECT_NAME} (${VERSION_FROM_LISP})")

# Create binary.
SET(MAIN_NAME "tools")
CONFIGURE_LISP_REDIRECT("${MAIN_NAME}"
                        "--load \"${CMAKE_CURRENT_SOURCE_DIR}/main/dump.lisp\"")
ADD_CUSTOM_COMMAND(OUTPUT  "${CMAKE_CURRENT_BINARY_DIR}/${NAME}"
                   COMMAND "${CMAKE_COMMAND}" -P "${CMAKE_CURRENT_BINARY_DIR}/sbcl.${MAIN_NAME}.cmake"
                   DEPENDS "${SCRIPT}"
                   COMMENT "Creating Lisp image ${NAME} (this can take a long time)")
ADD_CUSTOM_TARGET(binary ALL DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/${MAIN_NAME}")

# Create symlinks.
IF(NOT CMAKE_SYSTEM_NAME STREQUAL "Windows")
    SET(TOOLS info logger call send)
    FOREACH(TOOL ${TOOLS})
        MESSAGE(STATUS "Configuring tool ${TOOL}")
        ADD_CUSTOM_COMMAND(OUTPUT  "${CMAKE_CURRENT_BINARY_DIR}/${TOOL}"
                           COMMAND "${CMAKE_COMMAND}" -E create_symlink "./${MAIN_NAME}" "${CMAKE_CURRENT_BINARY_DIR}/${TOOL}"
                           DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/${MAIN_NAME}"
                           COMMENT "Creating symlink ${TOOL} -> ${MAIN_NAME}")
    ENDFOREACH()
    ADD_CUSTOM_TARGET(links ALL DEPENDS ${TOOLS})
ENDIF()

# Tests
ENABLE_TESTING()

# Lift-based tests for formatting and stats systems.
FOREACH(SYSTEM common formatting stats)
    CONFIGURE_LISP_REDIRECT("test-${SYSTEM}"
                            "--eval \"(proclaim (quote (optimize (debug 3) (safety 3) (speed 0))))\"
                             --eval \"(asdf:test-system :cl-rsb-${SYSTEM})\"")
    ADD_TEST(NAME    "test-${SYSTEM}"
             COMMAND "${CMAKE_COMMAND}" -P "${CMAKE_CURRENT_BINARY_DIR}/sbcl.test-${SYSTEM}.cmake")
    SET_TESTS_PROPERTIES("test-${SYSTEM}" PROPERTIES RUN_SERIAL TRUE)
ENDFOREACH()

# --help, --help-all and --version for all binaries.
FOREACH(TOOL ${TOOLS})
    ADD_TEST(NAME    "${TOOL}/help"
             COMMAND "${CMAKE_CURRENT_BINARY_DIR}/${TOOL}" --help)
    ADD_TEST(NAME    "${TOOL}/help-all"
             COMMAND "${CMAKE_CURRENT_BINARY_DIR}/${TOOL}" --help-for=all)
    ADD_TEST(NAME    "${TOOL}/version"
             COMMAND "${CMAKE_CURRENT_BINARY_DIR}/${TOOL}" --version)
ENDFOREACH()

# All categories for info binary.
FOREACH(CATEGORY verbose configuration connectors converters filters event-processing)
    ADD_TEST(NAME    "info/category=${CATEGORY}"
             COMMAND "${CMAKE_CURRENT_BINARY_DIR}/info" --debug --${CATEGORY})
ENDFOREACH()

# All error policies for logger, send and call binaries.
FOREACH(POLICY continue abort)
    ADD_TEST(NAME   "logger/on-error=${POLICY}"
            COMMAND "${CMAKE_CURRENT_BINARY_DIR}/logger"
                    --debug --on-error "${POLICY}"
                    --eval "(let ((timer (sb-ext:make-timer (lambda () (sb-ext:exit))))) (sb-ext:schedule-timer timer 3))"
                     inprocess:)
    ADD_TEST(NAME   "send/on-error=${POLICY}"
            COMMAND "${CMAKE_CURRENT_BINARY_DIR}/send"
                    --debug --on-error "${POLICY}"
                    1 inprocess:)
    ADD_TEST(NAME   "call/on-error=${POLICY}"
            COMMAND "${CMAKE_CURRENT_BINARY_DIR}/call"
                    --debug --on-error "${POLICY}"
                    --no-wait
                    "inprocess:/foo/bar(1)")
ENDFOREACH()

# All styles for logger and call binaries.
# TODO: add these:
# programmable/template :template #p"${CMAKE_CURRENT_SOURCE_DIR}/test/data/empty.template"
# programmable/script   :template #p"${CMAKE_CURRENT_SOURCE_DIR}/test/data/empty.lisp"
FOREACH(STYLE discard meta-data payload detailed
              compact statistics
              monitor monitor/scope monitor/origin monitor/type monitor/size
              timeline/scope timeline/origin)
    ADD_TEST(NAME    "logger/style=${STYLE}"
             COMMAND "${CMAKE_CURRENT_BINARY_DIR}/logger"
                     --debug --style "${STYLE}"
                     --eval "(let ((timer (sb-ext:make-timer (lambda () (sb-ext:exit))))) (sb-ext:schedule-timer timer 3))"
                     inprocess:)
    ADD_TEST(NAME   "call/style=${STYLE}/no-wait"
            COMMAND "${CMAKE_CURRENT_BINARY_DIR}/call"
                    --debug --style "${STYLE}"
                    --no-wait
                    "inprocess:/foo/bar(1)")
    ADD_TEST(NAME   "call/style=${STYLE}/wait"
            COMMAND "${CMAKE_CURRENT_BINARY_DIR}/call"
                    --debug --style "${STYLE}"
                    --eval "(progn (bt:make-thread (lambda () (rsb.patterns:with-local-server (s \"inprocess:/foo\") (rsb.patterns:with-methods (s) ((\"bar\" (x integer) x)) (sleep 2))))) (sleep 1))"
                    "inprocess:/foo/bar(1)")
ENDFOREACH()

# Installation
SET(BINARY_PREFIX    "rsb-")
SET(VERSION_SUFFIX   "cl${RSB_TOOLS_VERSION_MAJOR}.${RSB_TOOLS_VERSION_MINOR}")
SET(MAIN_BINARY_NAME ${BINARY_PREFIX}${MAIN_NAME}${VERSION_SUFFIX})
INSTALL(PROGRAMS    "${CMAKE_CURRENT_BINARY_DIR}/${MAIN_NAME}"
        DESTINATION "bin"
        RENAME      "${MAIN_BINARY_NAME}")
# Fake binary for symlink creation
IF(NOT CMAKE_SYSTEM_NAME STREQUAL "Windows")
    ADD_CUSTOM_COMMAND(OUTPUT  "${CMAKE_CURRENT_BINARY_DIR}/${MAIN_BINARY_NAME}"
                       COMMAND "${CMAKE_COMMAND}" -E touch "${CMAKE_CURRENT_BINARY_DIR}/${MAIN_BINARY_NAME}"
                       COMMENT "Creating fake link target ${MAIN_BINARY_NAME}")
    SET(TOOLS_VERSIONED "")
    FOREACH(TOOL ${TOOLS})
        SET(TOOL_NAME "${BINARY_PREFIX}${TOOL}${VERSION_SUFFIX}")
        SET(TOOLS_VERSIONED "${CMAKE_CURRENT_BINARY_DIR}/${TOOL_NAME};${TOOLS_VERSIONED}")
        INSTALL(CODE "MESSAGE(\"Creating symlink ${TOOL_NAME} -> ${MAIN_BINARY_NAME}\")")
        ADD_CUSTOM_COMMAND(OUTPUT  "${CMAKE_CURRENT_BINARY_DIR}/${TOOL_NAME}"
                           COMMAND "${CMAKE_COMMAND}" -E create_symlink "./${MAIN_BINARY_NAME}" "${CMAKE_CURRENT_BINARY_DIR}/${TOOL_NAME}"
                           DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/${MAIN_NAME}"
                                   "${CMAKE_CURRENT_BINARY_DIR}/${MAIN_BINARY_NAME}"
                           COMMENT "Creating symlink ${TOOL_NAME} -> ./${MAIN_BINARY_NAME}")
        INSTALL(FILES       "${CMAKE_CURRENT_BINARY_DIR}/${TOOL_NAME}"
                DESTINATION "bin")
    ENDFOREACH()
    ADD_CUSTOM_TARGET(links_versioned ALL DEPENDS ${TOOLS_VERSIONED})
ENDIF()

# Packaging
SET(PACKAGE_BASE_NAME           "${CMAKE_PROJECT_NAME}-${VERSION_SUFFIX}")
SET(CPACK_PACKAGE_VERSION_MAJOR ${RSB_TOOLS_VERSION_MAJOR})
SET(CPACK_PACKAGE_VERSION_MINOR ${RSB_TOOLS_VERSION_MINOR})
SET(CPACK_PACKAGE_VERSION_PATCH ${RSB_TOOLS_VERSION_PATCH})
SET(CPACK_PACKAGE_VERSION       "${CPACK_PACKAGE_VERSION_MAJOR}.${CPACK_PACKAGE_VERSION_MINOR}.${CPACK_PACKAGE_VERSION_PATCH}")
SET(CPACK_PACKAGE_VENDOR        "CoR-Lab, Bielefeld University")
SET(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_SOURCE_DIR}/COPYING")

INCLUDE("${CMAKE_SOURCE_DIR}/cpack/CPackInclude.cmake")

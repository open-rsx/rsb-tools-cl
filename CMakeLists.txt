# CMakeLists.txt --- CMake build description file.
#
# Copyright (C) 2011, 2012 Jan Moringen
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

# Project definition (version is grovelled from an asd system definition)
PROJECT(cl-rsb-tools)
SET(SYSTEM_NAME "cl-rsb-tools-logger")
MACRO(EXTRACT_COMPONENT NAME OUTPUT)
    FILE(STRINGS "${CMAKE_SOURCE_DIR}/${SYSTEM_NAME}.asd" TEMP
         REGEX   "defconstant +\\+version-${NAME}\\+")
    STRING(REGEX REPLACE ".*\\+version-${NAME}\\+ +([0-9]+).*" "\\1"
           CL_RSB_TOOLS_VERSION_${OUTPUT} ${TEMP})
ENDMACRO()
EXTRACT_COMPONENT(major    MAJOR)
EXTRACT_COMPONENT(minor    MINOR)
EXTRACT_COMPONENT(revision PATCH)

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

# Create binary.
SET(MAIN_NAME "tools")
SET(SCRIPT    "${CMAKE_CURRENT_SOURCE_DIR}/main/dump.lisp")
SET(DO        "--load \"${SCRIPT}\"")
SET(NAME      "tools")
IF(NOT CMAKE_SYSTEM_NAME STREQUAL "Windows")
    SET(REDIRECTIONS "OUTPUT_FILE \"${CMAKE_CURRENT_BINARY_DIR}/${NAME}.log\"
                      ERROR_FILE  \"${CMAKE_CURRENT_BINARY_DIR}/${NAME}.log\"")
ENDIF()
CONFIGURE_FILE("${CMAKE_CURRENT_SOURCE_DIR}/sbcl.cmake.in"
               "${CMAKE_CURRENT_BINARY_DIR}/sbcl.cmake"
               @ONLY)
ADD_CUSTOM_COMMAND(OUTPUT  "${CMAKE_CURRENT_BINARY_DIR}/${NAME}"
                   COMMAND "${CMAKE_COMMAND}" -P "${CMAKE_CURRENT_BINARY_DIR}/sbcl.cmake"
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

FOREACH(NAME formatting stats)
    SET(DO "--eval \"(proclaim (quote (optimize (debug 3) (safety 3) (speed 0))))\"
            --eval \"(asdf:test-system :cl-rsb-${NAME})\"
            --quit")
    IF(NOT CMAKE_SYSTEM_NAME STREQUAL "Windows")
        SET(REDIRECTIONS "OUTPUT_FILE \"${CMAKE_CURRENT_BINARY_DIR}/test-${NAME}.log\"
                          ERROR_FILE  \"${CMAKE_CURRENT_BINARY_DIR}/test-${NAME}.log\"")
    ENDIF()
    CONFIGURE_FILE("${CMAKE_CURRENT_SOURCE_DIR}/sbcl.cmake.in"
                   "${CMAKE_CURRENT_BINARY_DIR}/sbcl.test-${NAME}.cmake"
                   @ONLY)
    ADD_TEST(NAME    "rsb-${NAME}"
             COMMAND "${CMAKE_COMMAND}" -P "${CMAKE_CURRENT_BINARY_DIR}/sbcl.test-${NAME}.cmake")
    SET_TESTS_PROPERTIES("rsb-${NAME}" PROPERTIES RUN_SERIAL TRUE)
ENDFOREACH()

FOREACH(TOOL ${TOOLS})
    ADD_TEST(NAME    "${TOOL}/help"
             COMMAND "${CMAKE_CURRENT_BINARY_DIR}/${TOOL}" --help)
    ADD_TEST(NAME    "${TOOL}/help-all"
             COMMAND "${CMAKE_CURRENT_BINARY_DIR}/${TOOL}" --help-for=all)
    ADD_TEST(NAME    "${TOOL}/version"
             COMMAND "${CMAKE_CURRENT_BINARY_DIR}/${TOOL}" --version)
ENDFOREACH()

FOREACH(CATEGORY verbose configuration connectors converters filters event-processing)
    ADD_TEST(NAME    "info/${CATEGORY}"
             COMMAND "${CMAKE_CURRENT_BINARY_DIR}/info" --debug --${CATEGORY})
ENDFOREACH()

# TODO: add these:
# programmable/template :template #p"${CMAKE_CURRENT_SOURCE_DIR}/test/data/empty.template"
# programmable/script   :template #p"${CMAKE_CURRENT_SOURCE_DIR}/test/data/empty.lisp"
FOREACH(STYLE discard meta-data payload detailed
              compact statistics
              monitor monitor/scope monitor/origin monitor/type monitor/size
              timeline/scope timeline/origin)
    ADD_TEST(NAME    "logger/${STYLE}"
             COMMAND "${CMAKE_CURRENT_BINARY_DIR}/logger"
                     --debug --style "${STYLE}"
                     --eval "(let ((timer (sb-ext:make-timer (lambda () (sb-ext:exit))))) (sb-ext:schedule-timer timer 3))"
                     inprocess:)
ENDFOREACH()

# Installation
SET(BINARY_PREFIX    "rsb-")
SET(VERSION_SUFFIX   "cl${CL_RSB_TOOLS_VERSION_MAJOR}.${CL_RSB_TOOLS_VERSION_MINOR}")
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
SET(PACKAGE_BASE_NAME           "rsb-tools-${VERSION_SUFFIX}")
SET(CPACK_PACKAGE_VERSION_MAJOR ${CL_RSB_TOOLS_VERSION_MAJOR})
SET(CPACK_PACKAGE_VERSION_MINOR ${CL_RSB_TOOLS_VERSION_MINOR})
SET(CPACK_PACKAGE_VERSION_PATCH ${CL_RSB_TOOLS_VERSION_PATCH})
SET(CPACK_PACKAGE_VERSION       "${CPACK_PACKAGE_VERSION_MAJOR}.${CPACK_PACKAGE_VERSION_MINOR}.${CPACK_PACKAGE_VERSION_PATCH}")
SET(CPACK_PACKAGE_VENDOR        "CoR-Lab, Bielefeld University")
SET(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_SOURCE_DIR}/COPYING")

INCLUDE("${CMAKE_SOURCE_DIR}/cpack/CPackInclude.cmake")
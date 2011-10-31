CMAKE_MINIMUM_REQUIRED(VERSION 2.8)

# Project definition (version is grovelled from an asd system definition)
PROJECT(cl-rsb-tools)
SET(VERSION_SYSTEM_NAME cl-rsb-tools-logger)
EXECUTE_PROCESS(COMMAND         sh -c "cat ${CMAKE_SOURCE_DIR}/${VERSION_SYSTEM_NAME}.asd   \\
                                         | grep -e 'defconstant +\\+version-major\\+'       \\
                                         | sed -re 's/.*\\+version-major\\+ +([0-9]+)/\\1/'"
                OUTPUT_VARIABLE CL_RSB_TOOLS_VERSION_MAJOR
		OUTPUT_STRIP_TRAILING_WHITESPACE)
EXECUTE_PROCESS(COMMAND         sh -c "cat ${CMAKE_SOURCE_DIR}/${VERSION_SYSTEM_NAME}.asd   \\
                                         | grep -e 'defconstant +\\+version-minor\\+'       \\
                                         | sed -re 's/.*\\+version-minor\\+ +([0-9]+)/\\1/'"
                OUTPUT_VARIABLE CL_RSB_TOOLS_VERSION_MINOR
		OUTPUT_STRIP_TRAILING_WHITESPACE)
EXECUTE_PROCESS(COMMAND         sh -c "cat ${CMAKE_SOURCE_DIR}/${VERSION_SYSTEM_NAME}.asd      \\
                                         | grep -e 'defconstant +\\+version-revision\\+'       \\
                                         | sed -re 's/.*\\+version-revision\\+ +([0-9]+)/\\1/'"
                OUTPUT_VARIABLE CL_RSB_TOOLS_VERSION_PATCH
		OUTPUT_STRIP_TRAILING_WHITESPACE)

# Find and check SBCL installation.
SET(SBCL_HOME $ENV{SBCL_HOME})
IF(NOT SBCL_HOME)
    MESSAGE(FATAL_ERROR "SBCL_HOME is not set. Set it.")
ENDIF()
FIND_PROGRAM(SBCL_EXECUTABLE sbcl
             PATHS "${SBCL_HOME}/../../bin"
	     NO_DEFAULT_PATH)

# Configure Lisp environment.
SET(LISP_INIT_FILE "" CACHE FILEPATH "File to load as Lisp init file.")
IF(LISP_INIT_FILE)
    SET(LISP_INIT "--userinit ${LISP_INIT_FILE}")
ENDIF()
SET(CL_SOURCE_REGISTRY "${CMAKE_CURRENT_SOURCE_DIR}//:")
SET(ASDF_OUTPUT_TRANSLATIONS "(:output-translations (t (\\\"${CMAKE_CURRENT_BINARY_DIR}/fasl-cache\\\" :implementation)) :ignore-inherited-configuration)")

# Create images.
FILE(GLOB SCRIPTS "*/dump.lisp")
SET(IMAGES)
FOREACH(SCRIPT ${SCRIPTS})
    STRING(REPLACE "//dump.lisp" "" IMAGE ${SCRIPT})
    GET_FILENAME_COMPONENT(NAME ${IMAGE} NAME)
    CONFIGURE_FILE("${CMAKE_CURRENT_SOURCE_DIR}/sbcl.cmake.in"
                   "${CMAKE_CURRENT_BINARY_DIR}/sbcl.${NAME}.cmake"
		   @ONLY)
    ADD_CUSTOM_COMMAND(OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/${NAME}"
                       COMMAND ${CMAKE_COMMAND} -P "${CMAKE_CURRENT_BINARY_DIR}/sbcl.${NAME}.cmake"
                       DEPENDS ${SCRIPT}
                       COMMENT "Creating Lisp image ${NAME} (this can take a long time)")
    LIST(APPEND IMAGES "${CMAKE_CURRENT_BINARY_DIR}/${NAME}")
ENDFOREACH()
ADD_CUSTOM_TARGET(scripts ALL DEPENDS ${IMAGES})

# Tests
ENABLE_TESTING()

FOREACH(IMAGE ${IMAGES})
    FILE(RELATIVE_PATH NAME "${CMAKE_CURRENT_BINARY_DIR}" "${IMAGE}")
    ADD_TEST(NAME    "${NAME}-help"
             COMMAND "${IMAGE}" --help)
    ADD_TEST(NAME    "${NAME}-help-all"
             COMMAND "${IMAGE}" --help-for=all)
    ADD_TEST(NAME    "${NAME}-version"
             COMMAND "${IMAGE}" --version)
ENDFOREACH()

# Installation
FOREACH(IMAGE ${IMAGES})
    FILE(RELATIVE_PATH NAME "${CMAKE_CURRENT_BINARY_DIR}" "${IMAGE}")
    INSTALL(PROGRAMS    "${IMAGE}"
            DESTINATION "bin"
	    RENAME      "rsb-${NAME}-cl")
ENDFOREACH()

# Packaging
IF(CMAKE_SYSTEM_NAME STREQUAL "Linux")
    SET(CPACK_PACKAGE_VERSION_MAJOR ${CL_RSB_TOOLS_VERSION_MAJOR})
    SET(CPACK_PACKAGE_VERSION_MINOR ${CL_RSB_TOOLS_VERSION_MINOR})
    SET(CPACK_PACKAGE_VERSION_PATCH ${CL_RSB_TOOLS_VERSION_PATCH})

    FIND_PROGRAM(LSB_RELEASE_EXECUTABLE lsb_release)
    SET(LSB_PROCESSOR_ARCH ${CMAKE_SYSTEM_PROCESSOR})
    EXECUTE_PROCESS(COMMAND         ${LSB_RELEASE_EXECUTABLE} -s -c
                    OUTPUT_VARIABLE TMP_LSB_CODENAME
                    OUTPUT_STRIP_TRAILING_WHITESPACE)
    STRING(TOLOWER ${TMP_LSB_CODENAME} LSB_CODENAME)
    EXECUTE_PROCESS(COMMAND         ${LSB_RELEASE_EXECUTABLE} -s -r
                    OUTPUT_VARIABLE TMP_LSB_RELEASE
                    OUTPUT_STRIP_TRAILING_WHITESPACE)
    STRING(TOLOWER ${TMP_LSB_RELEASE} LSB_RELEASE)
    EXECUTE_PROCESS(COMMAND         ${LSB_RELEASE_EXECUTABLE} -s -i
                    OUTPUT_VARIABLE TMP_LSB_DISTRIBUTOR_ID
                    OUTPUT_STRIP_TRAILING_WHITESPACE)
    STRING(TOLOWER ${TMP_LSB_DISTRIBUTOR_ID} LSB_DISTRIBUTOR_ID)

    SET(CPACK_PACKAGE_VERSION       "${CPACK_PACKAGE_VERSION_MAJOR}.${CPACK_PACKAGE_VERSION_MINOR}.${CPACK_PACKAGE_VERSION_PATCH}")
    SET(CPACK_PACKAGE_FILE_NAME     "${CMAKE_PROJECT_NAME}-${CPACK_PACKAGE_VERSION}_${LSB_CODENAME}_${LSB_PROCESSOR_ARCH}")
    SET(CPACK_PACKAGE_VENDOR        "CoR-Lab Bielefeld University")
    SET(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_SOURCE_DIR}/COPYING")

    SET(CPACK_GENERATOR                  "DEB")
    SET(CPACK_DEBIAN_PACKAGE_NAME        "rsb-tools-cl")
    SET(CPACK_DEBIAN_PACKAGE_VERSION     "${CPACK_PACKAGE_VERSION}")
    SET(CPACK_DEBIAN_PACKAGE_MAINTAINER  "Jan Moringen (jmoringe@techfak.uni-bielefeld.de)")
    SET(CPACK_DEBIAN_PACKAGE_DESCRIPTION "Tools for the Robotics Service Bus (Common Lisp implementation)")
    SET(CPACK_DEBIAN_PACKAGE_PRIORITY    "optional")
    SET(CPACK_DEBIAN_PACKAGE_SECTION     "devel")
    SET(CPACK_DEBIAN_ARCHITECTURE        "${CMAKE_SYSTEM_PROCESSOR}")
    SET(CPACK_DEBIAN_PACKAGE_DEPENDS     "libc6")
    SET(CPACK_DEBIAN_PACKAGE_RECOMMENDS  "spread")

    MESSAGE("Debian Package: ${CPACK_DEBIAN_PACKAGE_NAME} ${CPACK_DEBIAN_PACKAGE_VERSION}")

    INCLUDE(CPack)
ENDIF()

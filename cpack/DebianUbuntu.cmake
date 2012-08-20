SET(CPACK_GENERATOR "DEB")

SET(CPACK_DEBIAN_PACKAGE_NAME        "${PACKAGE_BASE_NAME}")
SET(CPACK_DEBIAN_PACKAGE_VERSION     "${CPACK_PACKAGE_VERSION}${CPACK_PACKAGE_REVISION}")
SET(CPACK_DEBIAN_PACKAGE_MAINTAINER  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>")
SET(CPACK_DEBIAN_PACKAGE_DESCRIPTION "Tools for the Robotics Service Bus (Common Lisp implementation)
 Currently the set of tools includes
  * info
  * logger
  * send
  * call")
SET(CPACK_DEBIAN_PACKAGE_PRIORITY    "optional")
SET(CPACK_DEBIAN_PACKAGE_SECTION     "devel")
#SET(CPACK_DEBIAN_ARCHITECTURE        "${CMAKE_SYSTEM_PROCESSOR}") # Debian uses different names here
SET(CPACK_DEBIAN_PACKAGE_DEPENDS     "libc6")
SET(CPACK_DEBIAN_PACKAGE_SUGGESTS    "libzmq1 (>= 2.1.9), spread (>= 4.0)")

# Generate postinst and prerm hooks
SET(PACKAGE_ALT_PRIORITY "100")

SET(POSTINST_SCRIPT      "${CMAKE_CURRENT_BINARY_DIR}/postinst")
SET(PRERM_SCRIPT         "${CMAKE_CURRENT_BINARY_DIR}/prerm")
FILE(WRITE "${POSTINST_SCRIPT}" "#!/bin/sh\n\nset -e\n")
FILE(WRITE "${PRERM_SCRIPT}"    "#!/bin/sh\n\nset -e\n")
FOREACH(TOOL ${TOOLS})
    FILE(APPEND "${POSTINST_SCRIPT}"
                "update-alternatives --install                       \\
                   /usr/bin/${BINARY_PREFIX}${TOOL}                  \\
                   ${BINARY_PREFIX}${TOOL}                           \\
                   /usr/bin/${BINARY_PREFIX}${TOOL}${VERSION_SUFFIX} \\
                   ${PACKAGE_ALT_PRIORITY}\n\n")
    FILE(APPEND "${PRERM_SCRIPT}"
                "update-alternatives --remove                            \\
                   ${BINARY_PREFIX}${TOOL}                               \\
                   /usr/bin/${BINARY_PREFIX}${TOOL}${VERSION_SUFFIX}\n\n")
ENDFOREACH()
EXECUTE_PROCESS(COMMAND chmod 755 "${POSTINST_SCRIPT}" "${PRERM_SCRIPT}")
SET(CPACK_DEBIAN_PACKAGE_CONTROL_EXTRA "${POSTINST_SCRIPT};${PRERM_SCRIPT}")

# Write license file
FILE(WRITE "${CMAKE_BINARY_DIR}/copyright"
     "Copyright (C) 2010-2012 ${CPACK_DEBIAN_PACKAGE_MAINTAINER}

   This software is free software: you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   This software is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this software. If not, see
   <http://www.gnu.org/licenses>.

On Debian systems, the complete text of the GNU General Public
License can be found in `/usr/share/common-licenses/GPL-3'.")
INSTALL(FILES "${CMAKE_BINARY_DIR}/copyright"
        DESTINATION "share/doc/${CPACK_DEBIAN_PACKAGE_NAME}")

# Generate required change log files.
FIND_PROGRAM(LSB_EXECUTABLE "lsb_release")
EXECUTE_PROCESS(COMMAND ${LSB_EXECUTABLE} --short --codename
                OUTPUT_VARIABLE LSB_CODENAME
                OUTPUT_STRIP_TRAILING_WHITESPACE)

EXECUTE_PROCESS(COMMAND ${GIT_EXECUTABLE}
                        log "--format=%ad  %an  <%ae>%n%n%w(76,8,10)%s%w(76,8,8)%n%n%b%n"
                        --date=short
                COMMAND gzip -9
                OUTPUT_FILE "${CMAKE_BINARY_DIR}/changelog.gz")
EXECUTE_PROCESS(COMMAND sh -c "echo -n \"sed -e '\" ; for c in $(${GIT_EXECUTABLE} rev-list --all -- \"${CMAKE_CURRENT_LIST_FILE}\") ; do echo -n \"s/$c/$(${GIT_EXECUTABLE} describe --tags $c | sed -re s/[^0-9]*\\([0-9]+\\)\\.\\([0-9]+\\)-\\([0-9]+\\)-.*/\\\\1.\\'\\$\\(\\(\\\\2+1\\)\\)\\'.\\\\3/)/\\;\" ; done ; echo \"'\""
                OUTPUT_VARIABLE COMMIT_TO_VERSION_SED_RULES)
EXECUTE_PROCESS(COMMAND ${GIT_EXECUTABLE}
                        log "--format=${CPACK_DEBIAN_PACKAGE_NAME} (%H) ${LSB_CODENAME}; urgency=low%n%n%w(76,8,10)%s%w(76,8,8)%n%n%b%n%n%w(200,1,1)-- %an <%ae>  %ad%n"
                        --date=rfc
                        -- "${CMAKE_CURRENT_LIST_FILE}"
                COMMAND sh -c ${COMMIT_TO_VERSION_SED_RULES}
                COMMAND gzip -9
                OUTPUT_FILE "${CMAKE_BINARY_DIR}/changelog.Debian.gz")
INSTALL(FILES "${CMAKE_BINARY_DIR}/changelog.gz"
              "${CMAKE_BINARY_DIR}/changelog.Debian.gz"
        DESTINATION "share/doc/${CPACK_DEBIAN_PACKAGE_NAME}")

SET(CPACK_PACKAGE_FILE_NAME "${CPACK_DEBIAN_PACKAGE_NAME}-${CPACK_DEBIAN_PACKAGE_VERSION}_${CMAKE_SYSTEM_PROCESSOR}")

MESSAGE(STATUS "Debian Package: ${CPACK_DEBIAN_PACKAGE_NAME} (${CPACK_DEBIAN_PACKAGE_VERSION}) [${CPACK_PACKAGE_FILE_NAME}.deb]")

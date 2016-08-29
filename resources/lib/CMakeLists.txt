# Glyphicons and Bootstrap

set(BOOTSTRAP_BASE_URL "https://maxcdn.bootstrapcdn.com/bootstrap")
set(BOOTSTRAP_VERSION  "3.3.5")

foreach(FILE css/bootstrap.min.css
             js/bootstrap.min.js
             fonts/glyphicons-halflings-regular.woff2)
    file(DOWNLOAD "${BOOTSTRAP_BASE_URL}/${BOOTSTRAP_VERSION}/${FILE}"
                  "${CMAKE_CURRENT_SOURCE_DIR}/bootstrap-${BOOTSTRAP_VERSION}/${FILE}")
endforeach()

# JQuery

set(JQUERY_BASE_URL "https://cdnjs.cloudflare.com/ajax/libs/jquery")
set(JQUERY_VERSION  "2.1.3")

file(DOWNLOAD "${JQUERY_BASE_URL}/${JQUERY_VERSION}/jquery.min.js"
              "${CMAKE_CURRENT_SOURCE_DIR}/jquery-${JQUERY_VERSION}/jquery.min.js")

# Flot

set(FLOT_BASE_URL "https://cdnjs.cloudflare.com/ajax/libs/flot")
set(FLOT_VERSION  "0.8.3")

foreach(FILE jquery.flot jquery.flot.pie)
    file(DOWNLOAD "${FLOT_BASE_URL}/${FLOT_VERSION}/${FILE}.min.js"
                  "${CMAKE_CURRENT_SOURCE_DIR}/flot-${FLOT_VERSION}/${FILE}.min.js")
endforeach()

# moment.js

set(MOMENT_BASE_URL "https://cdnjs.cloudflare.com/ajax/libs/moment.js")
set(MOMENT_VERSION  "2.14.1")

file(DOWNLOAD "${MOMENT_BASE_URL}/${MOMENT_VERSION}/moment.min.js"
              "${CMAKE_CURRENT_SOURCE_DIR}/moment-${MOMENT_VERSION}/moment.min.js")

cmake_minimum_required(VERSION 2.6 FATAL_ERROR)

project()
string(TOLOWER ${PROJECT_NAME} PROJECT_NAME_LOWER)

include(${PROJECT_SOURCE_DIR}/cmake/rtcomp_utils.cmake)

# Set version and OS-specific settings
set(RTC_VERSION 1.0.0 CACHE STRING "<COMP> version")
DISSECT_VERSION()
GET_OS_INFO()

# Add an "uninstall" target
CONFIGURE_FILE ("${PROJECT_SOURCE_DIR}/cmake/uninstall_target.cmake.in"
    "${PROJECT_BINARY_DIR}/uninstall_target.cmake" IMMEDIATE @ONLY)
ADD_CUSTOM_TARGET (uninstall "${CMAKE_COMMAND}" -P
    "${PROJECT_BINARY_DIR}/uninstall_target.cmake")

# Component source files
set(comp_srcs comp/rtc.cpp
              comp/service_impl.cpp)
set(standalone_srcs comp/standalone.cpp)
set(idls)

# Compile the built-in IDL files
COMPILE_IDL_FILES(${PROJECT_SOURCE_DIR}/idl ${idls})
FILTER_LIST("${_idl_srcs}" "hh$" _installable_idl_srcs)
install(FILES ${_installable_idl_srcs}
    DESTINATION ${RTC_INCLUDE_INSTALL_DIR}/idl COMPONENT headers)
INSTALL_IDL_FILES(${PROJECT_SOURCE_DIR}/idl ${RTC_INCLUDE_INSTALL_DIR}/idl
    ${idls})

# Get necessary dependency information
GET_PKG_CONFIG_INFO(openrtm-aist REQUIRED)

# Set include and link directories and flags
include_directories(${PROJECT_SOURCE_DIR} ${PROJECT_BINARY_DIR})
APPLY_PKG_CONFIG_DIRS(openrtm-aist)
APPLY_PKG_CONFIG_TO_SRCS(openrtm-aist ${comp_srcs} ${proxy_srcs})

# Component target
add_library(rtc SHARED ${comp_srcs} ${proxy_srcs} ${_idl_srcs})
add_executable(rtc_standalone ${standalone_srcs})

target_link_libraries(rtc_standalone rtc)
APPLY_PKG_CONFIG_TO_TGTS(openrtm-aist rtc)

install(TARGETS rtc LIBRARY DESTINATION ${RTC_LIB_INSTALL_DIR}
    COMPONENT component)
install(TARGETS rtc_standalone RUNTIME DESTINATION bin
    COMPONENT component)

# Subdirectories
add_subdirectory(cmake)
add_subdirectory(comp)
add_subdirectory(doc)
add_subdirectory(examples)
add_subdirectory(idl)

# Package creation
include(InstallRequiredSystemLibraries)
set(RTC_DESCRIPTION "RT-Component for .")
set(RTC_VENDOR "Geoffrey Biggs, AIST")
set(RTC_EXECUTABLES "rtc__standalone" "RTC:")
configure_file("${PROJECT_SOURCE_DIR}/cmake/cpack_options.cmake.in"
    "${PROJECT_BINARY_DIR}/cpack_options.cmake" @ONLY)
set(CPACK_PROJECT_CONFIG_FILE "${PROJECT_BINARY_DIR}/cpack_options.cmake")
include(cpack)


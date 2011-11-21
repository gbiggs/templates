set(idls ${CMAKE_CURRENT_SOURCE_DIR}/<interface>.idl)

install(FILES ${idls} DESTINATION ${INC_INSTALL_DIR}/idl
    COMPONENT idl)

OPENRTM_COMPILE_IDL_FILES(${idls})
set(ALL_IDL_SRCS ${ALL_IDL_SRCS} PARENT_SCOPE)
FILTER_LIST("${ALL_IDL_SRCS}" "hh$" idl_headers)
install(FILES ${idl_headers}
    DESTINATION ${INC_INSTALL_DIR}/${PROJECT_NAME_LOWER}/idl
    COMPONENT headers)


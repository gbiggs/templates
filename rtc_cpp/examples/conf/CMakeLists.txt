set(comp_conf_in ${CMAKE_CURRENT_SOURCE_DIR}/${PROJECT_NAME_LOWER}.conf.in)
set(comp_conf ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME_LOWER}.conf)
configure_file(${comp_conf_in} ${comp_conf})

set(example_conf_files rtc.conf
                       ${comp_conf})

install(FILES ${example_conf_files}
    DESTINATION ${SHARE_INSTALL_DIR}/examples/conf COMPONENT examples)


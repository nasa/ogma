{{=<% %>=}}
###########################################################
#
# <%app_name_uc%> platform build setup
#
# This file is evaluated as part of the "prepare" stage
# and can be used to set up prerequisites for the build,
# such as generating header files
#
###########################################################

# The list of header files that control the <%app_name_uc%> configuration
set(<%app_name_uc%>_PLATFORM_CONFIG_FILE_LIST
  <%app_name_lc%>_internal_cfg.h
  <%app_name_lc%>_platform_cfg.h
  <%app_name_lc%>_perfids.h
  <%app_name_lc%>_msgids.h
)

# Create wrappers around the all the config header files
# This makes them individually overridable by the missions, without modifying
# the distribution default copies
foreach(<%app_name_uc%>_CFGFILE ${<%app_name_uc%>_PLATFORM_CONFIG_FILE_LIST})
  get_filename_component(CFGKEY "${<%app_name_uc%>_CFGFILE}" NAME_WE)
  if (DEFINED <%app_name_uc%>_CFGFILE_SRC_${CFGKEY})
    set(DEFAULT_SOURCE GENERATED_FILE "${<%app_name_uc%>_CFGFILE_SRC_${CFGKEY}}")
  else()
    set(DEFAULT_SOURCE FALLBACK_FILE "${CMAKE_CURRENT_LIST_DIR}/config/default_${<%app_name_uc%>_CFGFILE}")
  endif()
  generate_config_includefile(
    FILE_NAME           "${<%app_name_uc%>_CFGFILE}"
    ${DEFAULT_SOURCE}
  )
endforeach()

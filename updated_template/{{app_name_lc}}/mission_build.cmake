{{=<% %>=}}
###########################################################
#
# <%app_name_uc%> mission build setup
#
# This file is evaluated as part of the "prepare" stage
# and can be used to set up prerequisites for the build,
# such as generating header files
#
###########################################################

# The list of header files that control the <%app_name_uc%> configuration
set(<%app_name_uc%>_MISSION_CONFIG_FILE_LIST
  <%app_name_lc%>_fcncodes.h
  <%app_name_lc%>_interface_cfg.h
  <%app_name_lc%>_mission_cfg.h
  <%app_name_lc%>_perfids.h
  <%app_name_lc%>_msg.h
  <%app_name_lc%>_msgdefs.h
  <%app_name_lc%>_msgstruct.h
  <%app_name_lc%>_tbl.h
  <%app_name_lc%>_tbldefs.h
  <%app_name_lc%>_tblstruct.h
  <%app_name_lc%>_topicids.h
)

if (CFE_EDS_ENABLED_BUILD)

  # In an EDS-based build, these files come generated from the EDS tool
  set(<%app_name_uc%>_CFGFILE_SRC_<%app_name_lc%>_interface_cfg "<%app_name_lc%>_eds_designparameters.h")
  set(<%app_name_uc%>_CFGFILE_SRC_<%app_name_lc%>_tbldefs       "<%app_name_lc%>_eds_typedefs.h")
  set(<%app_name_uc%>_CFGFILE_SRC_<%app_name_lc%>_tblstruct     "<%app_name_lc%>_eds_typedefs.h")
  set(<%app_name_uc%>_CFGFILE_SRC_<%app_name_lc%>_msgdefs       "<%app_name_lc%>_eds_typedefs.h")
  set(<%app_name_uc%>_CFGFILE_SRC_<%app_name_lc%>_msgstruct     "<%app_name_lc%>_eds_typedefs.h")
  set(<%app_name_uc%>_CFGFILE_SRC_<%app_name_lc%>_fcncodes      "<%app_name_lc%>_eds_cc.h")

endif(CFE_EDS_ENABLED_BUILD)

# Create wrappers around the all the config header files
# This makes them individually overridable by the missions, without modifying
# the distribution default copies
foreach(<%app_name_uc%>_CFGFILE ${<%app_name_uc%>_MISSION_CONFIG_FILE_LIST})
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

# Register the standard build
set(SOURCE_FILES
  "${CMAKE_CURRENT_LIST_DIR}/copilot.c"
  "${CMAKE_CURRENT_LIST_DIR}/copilot.h"
  "${CMAKE_CURRENT_LIST_DIR}/copilot_types.h"
  "${CMAKE_CURRENT_LIST_DIR}/Copilot.cpp"
  "${CMAKE_CURRENT_LIST_DIR}/Copilot.fpp"
)
register_fprime_module()

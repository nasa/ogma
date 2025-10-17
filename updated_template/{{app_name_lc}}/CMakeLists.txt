project(CFE_{{app_name_uc}} C)

set(APP_SRC_FILES
  fsw/src/{{app_name_lc}}.c
  fsw/src/{{app_name_lc}}_cmds.c
  fsw/src/{{app_name_lc}}_utils.c
  fsw/src/copilot.c
  fsw/src/copilot.h
  fsw/src/copilot_types.h
)

if (CFE_EDS_ENABLED_BUILD)
  list(APPEND APP_SRC_FILES
    fsw/src/{{app_name_lc}}_eds_dispatch.c
  )
else()
  list(APPEND APP_SRC_FILES
    fsw/src/{{app_name_lc}}_dispatch.c
  )
endif()

# Create the app module
add_cfe_app({{app_name_lc}} ${APP_SRC_FILES})

add_custom_command(
    OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/fsw/src/cabal.sandbox.config
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/fsw/src/
    COMMAND cabal v1-sandbox init
    COMMAND cabal update
    COMMAND cabal v1-install copilot
    COMMENT "Installing Copilot"
)

add_custom_command(
   DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/fsw/src/cabal.sandbox.config
   DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/fsw/src/Properties.hs
   OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/fsw/src/copilot.c
          ${CMAKE_CURRENT_SOURCE_DIR}/fsw/src/copilot.h
          ${CMAKE_CURRENT_SOURCE_DIR}/fsw/src/copilot_types.h
   WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/fsw/src/
   COMMAND cabal v1-exec "--" runhaskell Properties.hs
   COMMENT "Compiling Copilot code"
)

add_custom_target(HASKELL_COPILOT
   DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/fsw/src/copilot.c
           ${CMAKE_CURRENT_SOURCE_DIR}/fsw/src/copilot.h
           ${CMAKE_CURRENT_SOURCE_DIR}/fsw/src/copilot_types.h
   SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/fsw/src/Properties.hs
)
add_dependencies({{app_name_lc}} HASKELL_COPILOT)

target_include_directories({{app_name_lc}} PUBLIC fsw/inc)
# Include the public API from sample_lib to demonstrate how
# to call library-provided functions
add_cfe_app_dependency({{app_name_lc}} sample_lib)

# Add table
add_cfe_tables({{app_name_lc}} fsw/tables/{{app_name_lc}}_tbl.c)

# If UT is enabled, then add the tests from the subdirectory
# Note that this is an app, and therefore does not provide
# stub functions, as other entities would not typically make
# direct function calls into this application.
if (ENABLE_UNIT_TESTS)
  add_subdirectory(unit-test)
endif (ENABLE_UNIT_TESTS)

cmake_minimum_required(VERSION 2.6.4)
project(CFE_COPILOT_APP C)

include_directories(../../Modules/Core/Interfaces)
{{#included_libraries}}
include_directories({{{.}}})
{{/included_libraries}}
include_directories(../inc)
include_directories(fsw/mission_inc)
include_directories(fsw/platform_inc)

aux_source_directory(fsw/src APP_SRC_FILES)

# Create the app module
add_cfe_app(copilot_cfs
  ${APP_SRC_FILES}
  ${CMAKE_CURRENT_SOURCE_DIR}/fsw/src/copilot.c
  ${CMAKE_CURRENT_SOURCE_DIR}/fsw/src/copilot.h
  ${CMAKE_CURRENT_SOURCE_DIR}/fsw/src/copilot_types.h
)

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
add_dependencies(copilot_cfs HASKELL_COPILOT)

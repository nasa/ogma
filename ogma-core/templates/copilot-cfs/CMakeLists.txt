cmake_minimum_required(VERSION 2.6.4)
project(CFE_SAMPLE_APP C)

include_directories(../../Modules/Core/Interfaces)
include_directories(../Icarouslib/fsw/platform_inc)
include_directories(../inc)
include_directories(fsw/mission_inc)
include_directories(fsw/platform_inc)

aux_source_directory(fsw/src APP_SRC_FILES)

# Create the app module
add_cfe_app(copilot_cfs ${APP_SRC_FILES})

add_custom_target(HASKELL_COPILOT COMMAND cabal v1-sandbox init COMMAND cabal update COMMAND cabal v1-install copilot COMMAND cabal v1-exec "--" ghc --make Properties.hs COMMAND ./Properties WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/fsw/src/ SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/fsw/src/Properties.hs)
add_dependencies(copilot_cfs HASKELL_COPILOT)

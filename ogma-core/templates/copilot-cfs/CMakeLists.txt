cmake_minimum_required(VERSION 2.6.4)
project(CFE_SAMPLE_APP C)

include_directories(../inc)
include_directories(fsw/mission_inc)
include_directories(fsw/platform_inc)

aux_source_directory(fsw/src APP_SRC_FILES)

# Create the app module
add_cfe_app(copilot_cfs ${APP_SRC_FILES})

add_custom_target(HASKELL_COPILOT COMMAND cabal sandbox init COMMAND cabal update COMMAND cabal install copilot COMMAND cabal exec "--" ghc --make Properties.hs COMMAND ./Properties WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/fsw/src/ SOURCES Properties.hs)
add_dependencies(copilot_cfs HASKELL_COPILOT)

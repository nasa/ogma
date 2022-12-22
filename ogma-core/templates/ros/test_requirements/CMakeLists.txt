cmake_minimum_required(VERSION 3.8)
project(test_requirements)

if(CMAKE_COMPILER_IS_GNUCXX OR CMAKE_CXX_COMPILER_ID MATCHES "Clang")
  add_compile_options(-Wall -Wextra -Wpedantic -Wno-unused-parameter -Wno-write-strings)
endif()

# find dependencies
find_package(ament_cmake REQUIRED)
find_package(rclcpp REQUIRED)
find_package(std_msgs REQUIRED)

# Uncomment to enable compiling the requirement tests
add_executable(test_requirements src/test_requirements.cpp)
ament_target_dependencies(test_requirements rclcpp std_msgs)

install(TARGETS
  test_requirements
  DESTINATION lib/${PROJECT_NAME})

if(BUILD_TESTING)
  find_package(ament_lint_auto REQUIRED)
  ament_lint_auto_find_test_dependencies()
endif()

ament_package()

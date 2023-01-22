cmake_minimum_required(VERSION 3.8)
project(copilot)

if(CMAKE_COMPILER_IS_GNUCXX OR CMAKE_CXX_COMPILER_ID MATCHES "Clang")
  add_compile_options(-Wall -Wextra -Wpedantic)
endif()

# find dependencies
find_package(ament_cmake REQUIRED)
find_package(rclcpp REQUIRED)
find_package(std_msgs REQUIRED)

add_executable(copilot src/copilot_monitor.cpp)
ament_target_dependencies(copilot rclcpp std_msgs)

# Uncomment to enable compiling the copilot logger
# add_executable(copilot_logger src/copilot_logger.cpp)
# ament_target_dependencies(copilot_logger rclcpp std_msgs)

install(TARGETS
  copilot
  DESTINATION lib/${PROJECT_NAME})

if(BUILD_TESTING)
  find_package(ament_lint_auto REQUIRED)
  ament_lint_auto_find_test_dependencies()
endif()

ament_package()

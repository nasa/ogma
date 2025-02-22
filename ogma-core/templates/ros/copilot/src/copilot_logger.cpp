#include <functional>
#include <memory>

#include "rclcpp/rclcpp.hpp"

#include "std_msgs/msg/bool.hpp"
#include "std_msgs/msg/empty.hpp"
#include "std_msgs/msg/u_int8.hpp"
#include "std_msgs/msg/u_int16.hpp"
#include "std_msgs/msg/u_int32.hpp"
#include "std_msgs/msg/u_int64.hpp"
#include "std_msgs/msg/int8.hpp"
#include "std_msgs/msg/int16.hpp"
#include "std_msgs/msg/int32.hpp"
#include "std_msgs/msg/int64.hpp"
#include "std_msgs/msg/float32.hpp"
#include "std_msgs/msg/float64.hpp"
#include "std_msgs/msg/empty.hpp"
using std::placeholders::_1;

class CopilotLogger : public rclcpp::Node {
  public:
    CopilotLogger() : Node("copilotlogger") {
{{#monitors}}
{{#monitorMsgType}}
      {{monitorName}}_subscription_ = this->create_subscription<{{.}}>(
        "copilot/{{monitorName}}", 10,
        std::bind(&CopilotLogger::{{monitorName}}_callback, this, _1));
{{/monitorMsgType}}
{{^monitorMsgType}}
      {{monitorName}}_subscription_ = this->create_subscription<std_msgs::msg::Empty>(
        "copilot/{{monitorName}}", 10,
        std::bind(&CopilotLogger::{{monitorName}}_callback, this, _1));
{{/monitorMsgType}}

{{/monitors}}
    }

  private:
{{#monitors}}
{{#monitorMsgType}}
    void {{monitorName}}_callback(const {{.}}::SharedPtr msg) const {
      RCLCPP_INFO(this->get_logger(), "Copilot monitor violation: {{monitorName}}");
    }
{{/monitorMsgType}}
{{^monitorMsgType}}
    void {{monitorName}}_callback(const std_msgs::msg::Empty::SharedPtr msg) const {
      RCLCPP_INFO(this->get_logger(), "Copilot monitor violation: {{monitorName}}");
    }
{{/monitorMsgType}}

{{/monitors}}
{{#monitors}}
{{#monitorMsgType}}
    rclcpp::Subscription<{{.}}>::SharedPtr {{monitorName}}_subscription_;
{{/monitorMsgType}}
{{^monitorMsgType}}
    rclcpp::Subscription<std_msgs::msg::Empty>::SharedPtr {{monitorName}}_subscription_;
{{/monitorMsgType}}

{{/monitors}}
};

int main(int argc, char* argv[]) {
  rclcpp::init(argc, argv);
  rclcpp::spin(std::make_shared<CopilotLogger>());
  rclcpp::shutdown();
  return 0;
}



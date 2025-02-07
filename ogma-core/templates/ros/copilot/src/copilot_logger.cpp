#include <functional>
#include <memory>

#include "rclcpp/rclcpp.hpp"

#include "std_msgs/msg/empty.hpp"
using std::placeholders::_1;

class CopilotLogger : public rclcpp::Node {
  public:
    CopilotLogger() : Node("copilotlogger") {
{{#monitors}}
      {{.}}_subscription_ = this->create_subscription<std_msgs::msg::Empty>(
        "copilot/{{.}}", 10,
        std::bind(&CopilotLogger::{{.}}_callback, this, _1));

{{/monitors}}
    }

  private:
{{#monitors}}
    void {{.}}_callback(const std_msgs::msg::Empty::SharedPtr msg) const {
      RCLCPP_INFO(this->get_logger(), "Copilot monitor violation: {{.}}");
    }

{{/monitors}}
{{#monitors}}
    rclcpp::Subscription<std_msgs::msg::Empty>::SharedPtr {{.}}_subscription_;

{{/monitors}}
};

int main(int argc, char* argv[]) {
  rclcpp::init(argc, argv);
  rclcpp::spin(std::make_shared<CopilotLogger>());
  rclcpp::shutdown();
  return 0;
}



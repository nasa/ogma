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
#include <cstdint>
#include "monitor.h"
#include "monitor.c"

using std::placeholders::_1;

{{#variables}}
{{varDeclType}} {{varDeclName}};
{{/variables}}

class CopilotRV : public rclcpp::Node {
  public:
    CopilotRV() : Node("copilotrv") {
      {{#variables}}
      {{varDeclName}}_subscription_ = this->create_subscription<{{varDeclMsgType}}>(
        "{{varDeclId}}", 10,
        std::bind(&CopilotRV::{{varDeclName}}_callback, this, _1));

      {{/variables}}
      {{#monitors}}
      {{.}}_publisher_ = this->create_publisher<std_msgs::msg::Empty>(
        "copilot/{{.}}", 10);

      {{/monitors}}
    }

{{#monitors}}
    // Report (publish) monitor violations.
    void {{.}}() {
      auto output = std_msgs::msg::Empty();
      {{.}}_publisher_->publish(output);
    }

{{/monitors}}
    // Needed so we can report messages to the log.
    static CopilotRV& getInstance() {
      static CopilotRV instance;
      return instance;
    }

  private:
    {{#variables}}
    void {{varDeclName}}_callback(const {{varDeclMsgType}}::SharedPtr msg) const {
      {{varDeclName}} = msg->data;
      step();
    }

    {{/variables}}
    {{#variables}}
    rclcpp::Subscription<{{varDeclMsgType}}>::SharedPtr {{varDeclName}}_subscription_;

    {{/variables}}
    {{#monitors}}
    rclcpp::Publisher<std_msgs::msg::Empty>::SharedPtr {{.}}_publisher_;

    {{/monitors}}
};

{{#monitors}}
// Pass monitor violations to the actual class, which has ways to
// communicate with other applications.
void {{.}}() {
  CopilotRV::getInstance().{{.}}();
}

{{/monitors}}
int main(int argc, char* argv[]) {
  rclcpp::init(argc, argv);
  rclcpp::spin(std::make_shared<CopilotRV>());
  rclcpp::shutdown();
  return 0;
}


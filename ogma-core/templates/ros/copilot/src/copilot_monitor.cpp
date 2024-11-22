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

{{{variablesS}}}
class CopilotRV : public rclcpp::Node {
  public:
    CopilotRV() : Node("copilotrv") {
{{{msgSubscriptionS}}}
{{{msgPublisherS}}}
    }

{{{msgHandlerInClassS}}}
    // Needed so we can report messages to the log.
    static CopilotRV& getInstance() {
      static CopilotRV instance;
      return instance;
    }

  private:
{{{msgCallbacks}}}
{{{msgSubscriptionDeclrs}}}
{{{msgPublisherDeclrs}}}
};

{{{msgHandlerGlobalS}}}
int main(int argc, char* argv[]) {
  rclcpp::init(argc, argv);
  rclcpp::spin(std::make_shared<CopilotRV>());
  rclcpp::shutdown();
  return 0;
}


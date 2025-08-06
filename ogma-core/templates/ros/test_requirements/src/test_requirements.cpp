#include <functional>
#include <memory>

#include "gtest/gtest.h"

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

using std::placeholders::_1;

class RequirementsTest : public rclcpp::Node {
  public:
    RequirementsTest() : Node("requirementstest") {

      declare_parameter("testing_seed", 0); // defaults to 0
      declare_parameter("testing_deadline", 2); // defaults to 2 secs


{{#testingVariables}}
      {{varDeclName}}_publisher_ = this->create_publisher<{{varDeclMsgType}}>(
        "{{varDeclId}}", 10);

{{/testingVariables}}

{{#monitors}}
      {{monitorName}}_subscription_ = this->create_subscription<std_msgs::msg::Empty>(
        "copilot/{{monitorName}}", 10,
        std::bind(&RequirementsTest::{{monitorName}}_callback, this, _1));

{{/monitors}}

      get_parameter("testing_seed", initial_seed);
      get_parameter("testing_deadline", deadline);

      std::srand((unsigned int)this->initial_seed);

      this->seed = this->initial_seed;
      this->max_tests = calculate_num_tests();

      rclcpp::Duration update_period = rclcpp::Duration::from_seconds(1);
      timerInit = rclcpp::create_timer(this->get_node_base_interface(),
                                       this->get_node_timers_interface(),
                                       this->get_node_clock_interface()->get_clock(),
                                       update_period,
                                       std::bind(&RequirementsTest::tests_init, this)
                                       );
    }

  private:


{{#monitors}}
    bool violation_{{monitorName}} = false;

{{/monitors}}
    bool violations = false;


{{#testingVariables}}
    rclcpp::Publisher<{{varDeclMsgType}}>::SharedPtr {{varDeclName}}_publisher_;

{{/testingVariables}}
{{#monitors}}
    void {{monitorName}}_callback(const std_msgs::msg::Empty::SharedPtr msg) {
        this->violation_{{monitorName}} = true;
        this->violations = true;
    }
{{/monitors}}

{{#monitors}}
    rclcpp::Subscription<std_msgs::msg::Empty>::SharedPtr {{monitorName}}_subscription_;

{{/monitors}}


    int initial_seed; // To be configured using a parameter.
    int seed;         // To be configured using a parameter.
    int deadline;     // To be configured using a parameter.

    int max_tests;
    int num_test = 0;

    // Calculate the number of tests to be executed
    int calculate_num_tests() {
       return abs(std::rand());
    }

    rclcpp::TimerBase::SharedPtr timerResult;
    rclcpp::TimerBase::SharedPtr timerInit;

    void tests_init () {
        timerInit->cancel();
        tests_step_send();
    }

    void tests_step_send () {

{{#testingVariables}}
       {{varDeclType}} {{varDeclName}}_data = {{varDeclRandom}}();
       auto {{varDeclName}}_data_msg = {{varDeclMsgType}}();
       {{varDeclName}}_data_msg.data = {{varDeclName}}_data;
       {{varDeclName}}_publisher_->publish({{varDeclName}}_data_msg);

{{/testingVariables}}


        rclcpp::Duration update_period = rclcpp::Duration::from_seconds(deadline);
        timerResult = rclcpp::create_timer(this->get_node_base_interface(),
                                           this->get_node_timers_interface(),
                                           this->get_node_clock_interface()->get_clock(),
                                           update_period,
                                           std::bind(&RequirementsTest::tests_step_result, this)
                                           );
    }

    void tests_step_result () {
        timerResult->cancel();

{{#monitors}}
        if (this->violation_{{monitorName}}) {
            this->publish_violation("{{monitorName}}");
        }

{{/monitors}}

       this->num_test++;

       // Stop if out of steps or there have been violations
       if ((this->num_test >= this->max_tests) || violations) {
         // Terminate using the gtest mechanism to indicate the result
         if (violations) {
           RCLCPP_INFO(this->get_logger(), "Tests failed");
           // FAIL();
         } else {
           RCLCPP_INFO(this->get_logger(), "Tests succeeded");
           // SUCCEED();
         }
         rclcpp::shutdown();
       } else {
         tests_step_send();
       }
    }

    float randomFloat() {
       int numerator = rand();
       int denominator = rand();

       // Ensure that we do not divide by zero.
       if (denominator == 0) {
           denominator = 1;
       }

       return (float)numerator / (float)denominator;
    }

    int randomInt() {
       return rand();
    }

    bool randomBool() {
       return rand() & 1;
    }

    void delay(int time) {
       rclcpp::sleep_for(std::chrono::seconds(time));
    }

    void publish_violation (const char* requirement) {
        RCLCPP_INFO(this->get_logger(), "Requirement violation. Req: %s; Seed: %d; Step: %d\\n",
            requirement, this->initial_seed, this->num_test);
    }
};

int main(int argc, char* argv[]) {
  rclcpp::init(argc, argv);
  rclcpp::spin(std::make_shared<RequirementsTest>());
  rclcpp::shutdown();
  return 0;
}

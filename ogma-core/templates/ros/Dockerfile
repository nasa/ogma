FROM osrf/space-ros:latest

ADD copilot /demo/copilot
ADD test_requirements /demo/test_requirements

USER root
RUN chown -R spaceros-user /demo/
USER spaceros-user
# WORKDIR /demo/copilot
# RUN source /home/spaceros-user/spaceros/install/setup.bash && cmake . && make
# WORKDIR /demo/test_requirements
# RUN source /home/spaceros-user/spaceros/install/setup.bash && cmake . && make
SHELL ["/bin/bash", "-c"]
WORKDIR /demo/
RUN source /home/spaceros-user/spaceros/install/setup.bash && colcon build --packages-select copilot --paths copilot/ --install-base /home/spaceros-user/spaceros/install/ && colcon build --packages-select test_requirements --paths test_requirements --install-base /home/spaceros-user/spaceros/install/

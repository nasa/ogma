FROM osrf/space-ros:humble-2024.10.0

ARG USER=spaceros-user
ARG PACKAGE_PATH=/home/${USER}/monitors
ARG ROS_PATH=/home/${USER}/spaceros/

RUN mkdir -p ${PACKAGE_PATH}/src/
ADD copilot ${PACKAGE_PATH}/src/copilot
ADD test_requirements ${PACKAGE_PATH}/src/test_requirements
USER root
RUN chown -R ${USER} ${PACKAGE_PATH}
USER ${USER}

SHELL ["/bin/bash", "-c"]
WORKDIR ${PACKAGE_PATH}
RUN source /opt/spaceros/install/setup.bash && \
    colcon build --packages-select copilot           && \
    colcon build --packages-select test_requirements

ADD screenrc /home/spaceros-user/.screenrc
USER root
ADD screenrc /root/.screenrc
RUN chown -R spaceros-user /home/spaceros-user/.screenrc
USER spaceros-user

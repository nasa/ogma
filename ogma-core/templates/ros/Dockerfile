FROM osrf/space-ros:latest

ARG USER=spaceros-user
ARG PACKAGE_PATH=/home/${USER}/monitors
ARG ROS_PATH=/home/${USER}/spaceros/

RUN mkdir -p ${PACKAGE_PATH}/src/
ADD copilot ${PACKAGE_PATH}/src/copilot
USER root
RUN chown -R ${USER} ${PACKAGE_PATH}
USER ${USER}

SHELL ["/bin/bash", "-c"]
WORKDIR ${PACKAGE_PATH}
RUN source ${ROS_PATH}/install/setup.bash && \
    colcon build

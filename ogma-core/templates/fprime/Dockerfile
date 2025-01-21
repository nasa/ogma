# This dockerfile compiles a monitoring application inside FPrime's Reference
# Application.
FROM ubuntu:focal

# Avoid questions during package installation.
ENV DEBIAN_FRONTEND=noninteractive

# Install FPrime dependencies and clone fprime from the repo.
RUN apt-get update
RUN apt-get install -y git cmake gcc python3 pip default-jre

RUN git clone https://github.com/nasa/fprime
RUN pip install -r fprime/requirements.txt

WORKDIR fprime/Ref

# Add all the monitoring app files.
RUN mkdir Copilot
ADD CMakeLists.txt Copilot/
ADD Copilot.fpp Copilot/
ADD Copilot.cpp Copilot/
ADD Copilot.hpp Copilot/
ADD copilot.c Copilot/
ADD copilot.h Copilot/
ADD copilot_types.h Copilot/

# Enable Copilot app (add it after SignalGen).
RUN sed -i -e '/^add_fprime_subdirectory.*SignalGen.*/a add_fprime_subdirectory("${CMAKE_CURRENT_LIST_DIR}\/Copilot\/")' CMakeLists.txt

RUN fprime-util generate

# Update Ref deployment.

## Define Component Instance.
##
## This command adds the contents of the given instance-copilot at the end of
## Queued component instances section, which is right before the Passive
## components section.
ADD instance-copilot .
RUN line=$(grep -n 'Passive component instances' Top/instances.fpp | tail -n1 | cut -d: -f1); line=$(($line - 2)); sed -i -e "${line}r instance-copilot" Top/instances.fpp
RUN rm instance-copilot

## Update topology.
##
## This command adds the copilot monitoring node right after linuxTime in the
## topology.
RUN sed -i -e '/^ \+instance linuxTime/a\ \ \ \ instance copilotMonitor' Top/topology.fpp

RUN fprime-util build --jobs "$(nproc || printf '%s\n' 1)"

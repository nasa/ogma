# Copyright 2021 United States Government as represented by the Administrator
# of the National Aeronautics and Space Administration. All Rights Reserved.
#
# Disclaimers
#
# No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY
# OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT
# LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO
# SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
# PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE
# SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF
# PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN
# ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR
# RECIPIENT OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR
# ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE. FURTHER,
# GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING
# THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES
# IT "AS IS."
#
# Waiver and Indemnity: RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST
# THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS
# ANY PRIOR RECIPIENT. IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN
# ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE,
# INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S
# USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE
# UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY
# PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW. RECIPIENT'S SOLE REMEDY
# FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS
# AGREEMENT.
FROM ubuntu:focal

# Do not ask questions during installation.
ENV DEBIAN_FRONTEND noninteractive

RUN apt-get update

# Enable HVR repo, for better control of GHC and Cabal compiler versions.
RUN apt-get install --yes software-properties-common
RUN add-apt-repository ppa:hvr/ghc
RUN apt-get update

# Dependencies needed to clone and compile repos. We separate the Haskell
# tools from other tools because their versions are likely to change and
# that helps avoid unnecessary re-installations.
RUN apt-get install --yes git libz-dev cmake build-essential
RUN apt-get install --yes ghc-8.8.4 cabal-install-3.0

# Set paths to find tools. There is a bug (?) in older versions of docker
# that makes the HOME variable not expand correctly inside the container.
# We work around that bug by explicitly setting the value.
ENV HOME=/root
ENV PATH=${HOME}/.cabal/bin:/opt/cabal/bin:/opt/ghc/bin:${PATH}

# Install Ogma.
RUN git clone https://github.com/nasa/ogma
RUN cabal update
RUN cabal v1-install alex happy
RUN cabal v1-install BNFC
RUN cabal v1-install ogma/ogma-**/

# Download ICAROUS.
RUN git clone https://github.com/nasa/icarous.git
RUN cd icarous && bash UpdateModules.sh

# Generate subapp in apps directory.
RUN echo position > /tmp/variables
RUN ogma cfs --app-target-dir=icarous/apps/copilot_cfs --variable-file=/tmp/variables

# Enable app in ICAROUS apps list. The following line adds the generated
# copilot app to the bottom of the list (right before the closing parenthesis).
RUN sed -i -e '/^)/i copilot_cfs' icarous/Icarous_defs/apps.cmake

# Compile complete system.
RUN cd icarous && make

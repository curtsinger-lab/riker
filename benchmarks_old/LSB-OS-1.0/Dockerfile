# Based on vscode development dockerfile from Microsoft
FROM ubuntu:20.04

ARG USERNAME=vscode
ARG USER_UID=1000
ARG USER_GID=$USER_UID

# Stop warnings from apt
ENV DEBIAN_FRONTEND=noninteractive

# Set root password as it will be needed later
RUN echo 'root:rewtpazz' | chpasswd

# setup dodo
RUN apt-get update \
    && apt-get -y install --no-install-recommends apt-utils dialog 2>&1 \
    # 
    # Install git, compilers, and other dev tools
    && apt-get -y install git gcc clang libfmt-dev time build-essential python3-cram graphviz \
    #
    # Create a non-root user to use if preferred - see https://aka.ms/vscode-remote/containers/non-root-user.
    && groupadd --gid $USER_GID $USERNAME \
    && useradd -s /bin/bash --uid $USER_UID --gid $USER_GID -m $USERNAME \
    # [Optional] Add sudo support for the non-root user
    && apt-get install -y sudo \
    && echo $USERNAME ALL=\(root\) NOPASSWD:ALL > /etc/sudoers.d/$USERNAME\
    && chmod 0440 /etc/sudoers.d/$USERNAME \
    #
    # Clean up
    && apt-get autoremove -y \
    && apt-get clean -y \
    && rm -rf /var/lib/apt/lists/*

# download and install dodo
COPY src /dodo/src

COPY deps /dodo/deps

COPY launch /dodo/launch

COPY Makefile /dodo/

RUN cd dodo \
    && make

# create benchmark dir
RUN mkdir -p /benchmark/LSB-OS-1.0

# install LSB-OS dependencies
RUN apt-get update \
    && apt-get -y install byacc

# install LSB-OS
COPY benchmarks/LSB-OS-1.0/install.sh \
    benchmarks/LSB-OS-1.0/lts_lsb-os-1.0.2.2.tgz \
    benchmarks/LSB-OS-1.0/tet_vsxgen_2.0.tgz \
    benchmarks/LSB-OS-1.0/setup_testsets.sh.patch \
    /benchmark/LSB-OS-1.0/

# install test suite
RUN cd /benchmark/LSB-OS-1.0 \
    && sh install.sh

# patch setup script to make it non-interactive
RUN cd /home/tet/test_sets \
    && patch < /benchmark/LSB-OS-1.0/setup_testsets.sh.patch

# switch to vsx0 user and continue installation
RUN sudo -u vsx0 -s \
    && /home/tet/setup.sh

# Go back to default apt frontend
ENV DEBIAN_FRONTEND=dialog

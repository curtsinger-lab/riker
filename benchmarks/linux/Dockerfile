# Based on vscode development dockerfile from Microsoft
FROM ubuntu:20.04

ARG USERNAME=vscode
ARG USER_UID=1000
ARG USER_GID=$USER_UID

# Stop warnings from apt
ENV DEBIAN_FRONTEND=noninteractive

# setup riker
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

# download git
RUN apt-get install git

# download and install riker
ADD src /riker/src

ADD deps /riker/deps

ADD launch /riker/launch

ADD Makefile /riker/

RUN cd riker \
    && make

# download wget
RUN apt-get update \
    && apt-get -y install wget

# install kernel build dependencies
RUN apt-get -y install flex bison bc rsync kmod cpio openssl libssl-dev dkms libelf-dev libudev-dev libpci-dev libiberty-dev autoconf lz4

# download and install the latest stable release of the linux kernel (5.9.6)
RUN mkdir -p benchmark \
    && cd benchmark \
    && wget https://cdn.kernel.org/pub/linux/kernel/v5.x/linux-5.9.6.tar.xz \
    && tar xf linux-5.9.6.tar.xz \
    && mv linux-5.9.6 linux

# copy an ordinary kernel config (from Ubuntu 20.04)
COPY benchmarks/linux/linux-kernel.config /benchmark/linux/.config

# copy Rikerfile
COPY benchmarks/linux/Rikerfile /benchmark/linux/

# add benchmarking scripts
COPY benchmarks/scripts/riker-run.sh benchmarks/scripts/make-run.sh /benchmark/

# Go back to default apt frontend
ENV DEBIAN_FRONTEND=dialog

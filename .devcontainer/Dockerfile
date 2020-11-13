# Based on vscode development dockerfile from Microsoft
FROM ubuntu:20.04

ARG USERNAME=vscode
ARG USER_UID=1000
ARG USER_GID=$USER_UID

# Stop warnings from apt
ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update \
    && apt-get -y install --no-install-recommends apt-utils dialog 2>&1 \
    # 
    # Install git, compilers, and other dev tools
    && apt-get -y install \
    git \
    gcc \
    gcc-9 \
    clang \
    gdb \
    lldb \
    liblldb-dev \
    llvm-dev \
    libfmt-dev \
    time \
    build-essential \
    python3-cram \
    python3-pip \
    graphviz \
    cmake \
    graphviz \
    #
    # Create a non-root user to use if preferred - see https://aka.ms/vscode-remote/containers/non-root-user.
    && groupadd --gid $USER_GID $USERNAME \
    && useradd -s /bin/bash --uid $USER_UID --gid $USER_GID -m $USERNAME \
    # [Optional] Add sudo support for the non-root user
    && apt-get install -y sudo \
    && echo $USERNAME ALL=\(root\) NOPASSWD:ALL > /etc/sudoers.d/$USERNAME\
    && chmod 0440 /etc/sudoers.d/$USERNAME \
    ## cram is called cram3 in 20.04
    && update-alternatives --install /usr/bin/cram cram /usr/bin/cram3 100 \
    #
    # Clean up
    && apt-get autoremove -y \
    && apt-get clean -y \
    && rm -rf /var/lib/apt/lists/*

# install lldb-mi for lldb debugging support in vscode
RUN mkdir /code \
    && cd /code \
    && git clone https://github.com/lldb-tools/lldb-mi.git \
    && cd lldb-mi \
    && cmake . \
    && cmake --build . \
    && rm -f /usr/bin/lldb-mi \
    && mv src/lldb-mi /usr/bin/lldb-mi \
    && ln -s /usr/bin/lldb-server-10 /usr/bin/lldb-server-10.0.0

# Go back to default apt frontend
ENV DEBIAN_FRONTEND=dialog

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
    git=1:2.25.1-1ubuntu3 \
    gcc=4:9.3.0-1ubuntu2 \
    clang=1:10.0-50~exp1 \
    gdb=9.1-0ubuntu1 \
    lldb=1:10.0-50~exp1 \
    liblldb-dev=1:10.0-50~exp1 \
    llvm-dev=1:10.0-50~exp1 \
    libfmt-dev=6.1.2+ds-2 \
    time=1.7-25.1build1 \
    build-essential=12.8ubuntu1 \
    python3-cram=0.7-4 \
    python3-pip=20.0.2-5ubuntu1.1 \
    graphviz=2.42.2-3build2 \
    cmake=3.16.3-1ubuntu1 \
    texlive-latex-base=2019.20200218-1 \
    lsb-release=11.1.0ubuntu2 \
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
    && git checkout 6257b475e30b50764e2dc98e51b3f1a217c13914 \
    && cmake . \
    && cmake --build . \
    && rm -f /usr/bin/lldb-mi \
    && mv src/lldb-mi /usr/bin/lldb-mi \
    && ln -s /usr/bin/lldb-server-10 /usr/bin/lldb-server-10.0.0

# Go back to default apt frontend
ENV DEBIAN_FRONTEND=dialog

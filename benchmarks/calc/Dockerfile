# Based on vscode development dockerfile from Microsoft
FROM ubuntu:19.10

ARG USERNAME=vscode
ARG USER_UID=1000
ARG USER_GID=$USER_UID

# Stop warnings from apt
ENV DEBIAN_FRONTEND=noninteractive

# setup dodo
RUN apt-get update \
    && apt-get -y install --no-install-recommends apt-utils dialog 2>&1 \
    # 
    # Install git, compilers, and other dev tools
    && apt-get -y install git gcc clang libfmt-dev time build-essential python-cram graphviz \
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

# install calc dependencies
RUN apt-get install libreadline-dev groff-base

# download and install the latest tagged relase of calc (2.12.7.2)
# and patch Makefile so that clang does not die on warnings
RUN git clone https://github.com/lcn2/calc.git \
    && cd calc \
    && git checkout 2.12.7.2

# Go back to default apt frontend
ENV DEBIAN_FRONTEND=dialog

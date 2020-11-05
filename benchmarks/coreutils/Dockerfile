# Based on vscode development dockerfile from Microsoft
FROM ubuntu:20.04

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

# download and install dodo
ADD src /dodo/src

ADD deps /dodo/deps

ADD launch /dodo/launch

ADD Makefile /dodo/

RUN cd dodo \
    && make

# download wget
RUN apt-get update \
    && apt-get install wget

# download and install the latest tagged release of coreutils (8.32)
RUN mkdir -p benchmark \
    && cd benchmark \
    && wget --no-clobber https://ftp.gnu.org/gnu/coreutils/coreutils-8.32.tar.gz \
    && tar xzvf coreutils-8.32.tar.gz \
    && mv coreutils-8.32 coreutils \
    && cd coreutils

# copy Dodofile
COPY benchmarks/coreutils/Dodofile /benchmark/coreutils/

# add benchmarking script
COPY benchmarks/scripts/run.sh /benchmark/

# Go back to default apt frontend
ENV DEBIAN_FRONTEND=dialog

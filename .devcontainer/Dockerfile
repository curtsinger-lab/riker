# Based on vscode development dockerfile from Microsoft
FROM arm64/ubuntu:20.04

ARG USERNAME=vscode
ARG USER_UID=1000
ARG USER_GID=$USER_UID

# Stop warnings from apt
ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update
RUN apt-get -y install --no-install-recommends apt-utils dialog 2>&1

# Install packages
RUN apt-get -y install --no-install-recommends \
    # dev dependencies
    make clang libfmt-dev gdb git \
    # test dependencies
    gcc python3-cram file graphviz texlive-latex-base \
    # bench dependencies
    cmake wget m4

# Link cram3 as cram
RUN update-alternatives --install /usr/bin/cram cram /usr/bin/cram3 100

# Create a non-root user
RUN groupadd --gid $USER_GID $USERNAME \
    && useradd -s /bin/bash --uid $USER_UID --gid $USER_GID -m $USERNAME \
    # [Optional] Add sudo support for the non-root user
    && apt-get install -y sudo \
    && echo $USERNAME ALL=\(root\) NOPASSWD:ALL > /etc/sudoers.d/$USERNAME\
    && chmod 0440 /etc/sudoers.d/$USERNAME

# Clean up
RUN apt-get autoremove -y \
    && apt-get clean -y \
    && rm -rf /var/lib/apt/lists/*

# Go back to default apt frontend
ENV DEBIAN_FRONTEND=dialog

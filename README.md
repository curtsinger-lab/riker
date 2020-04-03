```
    .--.
.--/ *  \
(--._   |
 `   /  /_.----._  _
    /      __    \| )
   (       \-      (
    \_     /='     |
      \___    ____.'
          \ \/
          | |
         '" "`
```
# `dodo`: Fast and Precise Automatic Build Management
Dodo is currently in a non-functional state, but user instructions will follow as functionality is restored. See the *Development Environment* section below for instructions on setting up your machine to work on Dodo.

## Development Environment
Dodo currently runs only on Linux. There are two supported development environments for Dodo, a native Linux machine and a Docker container. See instructions below for setting up each environment.

### Development on a Linux Machine (or Virtual Machine)
Set up build dependencies:
```
$ sudo apt install git gcc clang build-essential python-cram graphviz
```

Clone the Dodo repository and its submodules:
```
$ git clone --recursive git@github.com:curtsinger-lab/dodo
```

Build Dodo with `make` and run the test suite:
```
$ cd dodo
$ make
$ make test
```

### Development in Docker
The Dodo repository is configured to work with Visual Studio Code, including support for remote development in Docker containers. You can work on Dodo inside a Docker container without VSCode, but you will have to build the container, check out code, and initiate connections to the container manually.

Following these instructions will give you a Docker container set up for Dodo development, with the Dodo source stored on the container's volume. Sharing source files from the host machine introduces significant overhead in Docker, but if you want that approach see the *Other Docker Options* section below.

If you are running macOS or Windows, install [Docker Desktop](https://www.docker.com/products/docker-desktop) for your platform. Linux users who opt to use Docker for development will need to install Docker Engine following [these instructions](https://docs.docker.com/install/).

Next, install the [Remote Development Extension Pack](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.vscode-remote-extensionpack) for Visual Studio Code.

Open the command palette in VS Code (<kbd>Command</kbd>+<kbd>Shift</kbd>+<kbd>P</kbd> in macOS). Launch the *Remote-Containers: Open Repository in Container* command by typing in enough of its name to find it in the command palette. This command will build a new container for Dodo development using the container configuration in this repository. At the prompt, enter in the path to this repository (`curtsinger-lab/dodo` if you are not working in a fork).

You should see a prompt asking what volume you want to clone the repository to. Unless you have a compelling reason to use some existing volume, choose *Create New Volume*.

Next, type the name of the directory you want the cloned repository to live in. This directory is inside the new container, so there should be no conflicts.

VSCode will now build the Docker container, which can take a few minutes. After that process has finished, open a terminal window in VSCode. The terminal should be running in the container, with the prompt `vscode@dev`. The `vscode` user is set up with `sudo` access in the container.

The last step before building is to check out git submodules:
```
$ git submodule init
$ git submodule update
```

You should now be able to build and test Dodo:
```
$ make
$ make test
```

Some VSCode extensions have to be installed in the Docker container to work correctly with remote development. The configuration in `.devcontainer/devcontainer.json` sets up the C/C++ extension, but you may want to install others as well. To set these up, open the *Extensions* tab in the activity bar (on the left in VSCode) and scroll through the list to look for grayed-out extensions. You can install them by clicking the *Install in Dev Container* button.

### Other Docker Options
You can use Docker for development on a source tree that is cloned to your host machine instead of inside the container. This is useful if you want other applications to access development files, but I/O from the container will run significantly slower, at least on macOS.

To set up this development environment, start by cloning the repository to your host machine:
```
$ git clone --recursive git@github.com:curtsinger-lab/dodo
```

Now, Open the cloned directory in VSCode. Open the command palette (<kbd>Command</kbd>+<kbd>Shift</kbd>+<kbd>P</kbd>) and launch the *Remote-Containers: Open Folder in Container* command. When a file browser window pops up, select the root of the cloned Dodo repository (this should be the default).

After a few minutes, the container should be built and ready to use. Keep in mind, building and running Dodo will be quite slow in this environment because file I/O is much slower when sharing files with the host system. It takes about twice as long to build Dodo in this configuration compared to the recommended Docker configuration (24 seconds vs. 12 seconds) on a 2015 MacBook Pro.

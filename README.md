# Riker: Don't `make`, Make It So
Riker is a forward build tool that automatically discovers incremental builds from simple specifications.
A paper describing Riker will appear at USENIX ATC'22, which will be accessible at <https://www.usenix.org/conference/atc22/presentation/curtsinger> after the conference begins.

Riker works on x86_64 and ARM64 Linux, although the ARM64 target is has seen much less testing.
Regardless of your platform, you should be aware that this is the product of research and will certainly contain bugs;
don't rely on Riker for mission-critical builds, but please do try it out and **file a bug report** if you run into issues.

## Getting Started
The Docker configuration in this repository works with VSCode's container development and GitHub Codespaces;
these should make it easy to get started with Riker if you just want to test it out.
If you want to use Riker on an existing Linux machine, the following steps should get things up and running:

First, install Riker's build and test dependencies (package names for Ubuntu 20.04):
```
$ sudo apt install make clang libfmt-dev gdb git gcc python3-cram file graphviz
```

Riker's test suite relies on the [cram](https://bitheap.org/cram/) tool.
Use `update-alternatives` to set it up:

```
$ sudo update-alternatives --install /usr/bin/cram cram /usr/bin/cram3 100
```

Now clone the Riker repository and its submodules:
```
$ git clone --recursive git@github.com:curtsinger-lab/riker
```

Build Riker with `make` and run the test suite:
```
$ cd riker
$ make
$ make test
```

If you see any failing tests something has gone wrong;
please file a bug report if you encounter issues here.
You can example the `.err` files under the `tests/*/` directories to see the output from the test suite.

There is no `install` target for Riker because the project is not yet ready for production use.
You can add `riker/debug/bin` to your `PATH` to use Riker elsewhere, or produce a release build with `make release` and use the output under the `release` directory.

## Using Riker
To use Riker, you first need a project to build.
For this example, we'll assume you are building a C project with a number of `.c` and `.h` files in the top-level directory.
To specify a build for Riker, create a Rikerfile like this one:

```
gcc -o myprogram *.c
```

Executing the Rikerfile as a shell script should perform a full build of the project;
you can even add a `#!/bin/sh` line to the top of the Rikerfile and make it executable, but this isn't required.

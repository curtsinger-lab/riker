# Riker: Don't `make`, Make It So

Riker is a new build tool.
Unlike `make`, Riker produces fast builds without detailed specifications.
Importantly, Riker never misses a dependency and always ensures that incremental builds are consistent with the full build.
In many cases, a single command such as `gcc *.c` suffices.

Riker was one of two papers that won "best paper" at USENIX ATC'22.
A paper, titled "Riker: Always-Correct and Fast Incremental Builds from Simple Specifications", is available at <https://www.usenix.org/conference/atc22/presentation/curtsinger>.
A talk video and slides will be available after the conference.

## Supported Platforms and Disclaimer

Riker works on x86_64 and ARM64 Linux, although the ARM64 target is has seen much less testing.
Regardless of your platform, you should be aware that this is an alpha-quality release.
Please do try it out and [file a bug report](https://github.com/curtsinger-lab/riker/issues) if you run into issues.

## License
Riker is available under the BSD three-clause license.
If you want to use Riker but this license interferes with your ability to do so, please contact the authors and we will try to help.

## Getting Started
The Docker configuration in this repository works with VSCode's container development and GitHub Codespaces;
these should make it easy to get started with Riker if you just want to test it out.
If you want to use Riker on an existing Linux machine, the following steps should get things up and running:

First, install Riker's build and test dependencies (package names for Ubuntu 20.04):
```
$ sudo apt install make clang llvm git gcc python3-cram file graphviz
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

Once the Riker repository has cloned, move to that directory and build Riker:
```
$ cd riker
$ make
```

After building Riker, we recommend running Riker's test suite. Riker may skip some tests, but it should pass all of the tests that it runs.
```
$ make test
```

You can examine the `.err` files under the `tests/*/` directories to see the output from the test suite. If you run into problems with Riker, please run our test suite and include that output with [a bug report](https://github.com/curtsinger-lab/riker/issues).

## Installing Riker
Riker is alpha-quality software, but if you want to install it globally you can do so with the following commands:

```
$ make
$ sudo make install
```

You can also install it without sudo to a different location by overriding the PREFIX in the Makefile:

```
$ make PREFIX=~/.local/ install
```

The above commands will build and install a debug version of Riker.
We recommend using the debug build because it is only marginally slower than a release build, but collects more information to help track down bugs if they do come up.
You can install a release build with `make release` and `sudo make install-release`.

You can run Riker's test suite against the installed version of Riker with the `test-installed` target:
```
$ make test-installed
```

## Using Riker
To use Riker, you first need a project to build.
For this example, we'll assume you are building a C project with a number of `.c` and `.h` files in the top-level directory.
To specify a build for Riker, create a Rikerfile like this one:

```
gcc -o myprogram *.c
```

Executing the Rikerfile as a shell script should perform a full build of the project;
you can even add a `#!/bin/sh` line to the top of the Rikerfile and make it executable, but this isn't required.

With the build specified, you can now run a full build with Riker (assuming you've put `rkr` in your `PATH`):
```
$ rkr --show
```

The `--show` flag is optional, but will print each command that runs during the build.

Now that you've run a full build, you can edit source files, delete targets, or even edit the `Rikerfile` and run `rkr` again to update the build.
Riker will only execute commands whose inputs have changed, so you should expect to see fewer commands in the output if you include the `--show` flag.

## Larger Builds
Real projects will typically have more complicated build procedures, but with Riker those builds are still simple to specify.
This repository includes a [Rikerfile](Rikerfile) to build Riker itself.
This build includes some typical platform detection and more complex compilation options, but is still quite a bit simpler than Riker's wildcard-heavy make build.

The source repository also includes Rikerfiles for a number of projects that were used in the evaluation for the Riker paper.
You can find Rikerfiles that build `redis`, `memcached`, `sqlite`, and `xz` under `benchmarks/*/files`.
Other benchmarks were only evaluated with full builds where Riker simply wraps the existing `make` build;
we don't expect these builds to perform quite as well on incremental updates, since `make` itself creates dependencies that Riker discovers.

You may also find it informative to browse through the test cases under the `tests/*` directories.
These tests, written for `cram`, are meant to be human-readable and demonstrate many of Riker's capabilities.

## Other Platforms
Unfortunately, Riker does not yet work on Windows or macOS.
We're interested in adding support for these platforms, but Riker relies on system call tracing that will need to be ported, and there are some unusual requirements that make this difficult.
If you are familiar with system call tracing (not library interposition) on these platforms and would like to help, please let us know.

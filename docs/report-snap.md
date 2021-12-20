# End of Semester Report for Snap Project

### 1. What was the goal for this task?

The goal for this task is to create a snap app of Riker for future open-source distribution. This tasks include adding a subcommeand that can automatically generates a `.snap` program that can be distributed and installed on other devices using 2 different approaches: `squashfs` and `snapcraft`. Additionally, it also includes an automatically generated snapcraft.yaml file which can be used to build a `.snap` app where the program can be built and run independently of its environment.  

### 2. Why is this useful?

As the next step after generating a list of dependencies of a program, we aim to build and containerized Riker into `.snap` app that could be publicly distributed later to the Linux community. By downloading and installing our snap, users can start using riker without having to deal with the hassle of cloning the giant codebase, compiling and debugging building problems, such as system release incompatibility, as well as saving time for the steep learning curve of using and creating `.snap`, `snapd`, `snapcraft`.

### 3. How does your implementation work?

#### What is in a Snap?
*Source: [Introduction to snapcraft](https://snapcraft.io/blog/introduction-to-snapcraft), [What is in a Snap?](https://snapcraft.io/blog/whats-in-a-snap)*

The Snap system consists of several major components:
- `Snapd` is a background service that allows users to install and run snaps.
- Snap is the userspace component of the `snapd` service. For instance, `snap install <foo>` will install the application named `<foo>` from an online store called the Snap store.
- The Snap Store is a central repository of snap applications. If offers security and cryptographic signatures. The store contents are available to any system running `snapd`. The store features release channels, which allow multiple versions of the application to be available at the same time. Developers who want to publish their applications as snaps need to register for a free account in the store.
- `Snapcraft` is the command line tool that allows developers to build and publish their applications as snaps.

In short, `Snapcraft` is the command line tool to build your program into a snap, which is created as the final artifact of the build process, with packages bearing a `.snap` extension. The snap file format is a single compressed SquashFS filesystem. It includes the application code (i.e. executable files) and declarative metadata (stored in `snapcraft.yaml`), which is interpreted by the `snapd` service to set up a secure sandbox for that application to run after installing from the store.


As mentioned above, we came up with 2 approaches to build the snap: `squashfs` and `snapcraft`.

#### snapcraft

We follow the strict structure of a stand `snapcraft.yaml` format by calling `snapcraft init`. If you are new to Snap, refer to this doc [*How to make your first snap*](https://snapcraft.io/blog/how-to-make-your-first-snap) or [*Create you first snap tutorial*](https://ubuntu.com/tutorials/create-your-first-snap#1-overview) to understand the structure, usage of keywords and scriplets in `snapcraft.yaml`.

Due to the complexity in the build of Riker, using the default `make` plugin of `snapcraft` doesn't work. Therefore, we resort to our own instructions of the build with `override-build`:
``` 
override-build: |
    make
    for file in `find . -maxdepth 1 -executable -type f` 
    do
        install -m 755 $file $SNAPCRAFT_PRIME
    done
    ... (installation of run-time packages)
```
This tells `snapcraft` to make Riker, install all executable files on the current directory (setting `-maxdept 1` restricts executable files from subdirectories, which are used during build time only). We then stage all the packages listed in `.rkr-deps` to serve during run-time:
```
stage-packages:
    # - libstdc++-9-dev
    - linux-headers-5.4.0-91-generic
    - clang
    - libc6-dev
    - libllvm10
    - libpcre2-8-0
    ...
    stage: 
    - usr/lib/locale/locale-archive
    - etc/ld.so.cache
```
Some packages are listed with `#` meaning they are required but appears to be invalid/non-existed, i.e. they couldn't be found and downloaded on the current release of Ubuntu, which will need more examination in the future.

Once we have constructed our `snapcraft.yaml` file, we can build our snap simply by running `snapcraft` on the cmd line, and install the snap to our system with `sudo snap install riker_0.1_amd64.snap`. However, the current problem of this approach is although riker is correctly build and compressed, when we try running riker with `riker.rkr`, we got:
```
(error) Failed to start tracing: Operation not permitted
Failure occurred in child process 90052
Sending termination signal to 90027
Caught termination signal in 90027
Backtrace: 
Backtrace: 
/snap/riker/x9/rkr[0x4bd194]
/snap/riker/x9/rkr[0x4bd194]
/snap/riker/x9/rkr[0x506522]
/snap/riker/x9/rkr[0x4bd153]
/snap/riker/x9/rkr[0x5030d9]
/snap/riker/x9/rkr[0x4cedf3]
/lib/x86_64-linux-gnu/libpthread.so.0(+0x153c0)[0x7ffff7c633c0]
/snap/riker/x9/rkr[0x5120f4]
/snap/riker/x9/rkr[0x59fe7e]
/snap/riker/x9/rkr[0x46200d]
/snap/riker/x9/rkr[0x47514b]
/snap/riker/x9/rkr[0x497f6e]
/lib/x86_64-linux-gnu/libc.so.6(wait4+0x1a)[0x7ffff7b41dba]
/snap/riker/x9/rkr[0x47cb81]
/snap/riker/x9/rkr[0x5054a9]
/snap/riker/x9/rkr[0x473d07]
/snap/riker/x9/rkr[0x5030d9]
/snap/riker/x9/rkr[0x4cedf3]
/snap/riker/x9/rkr[0x5120f4]
/snap/riker/x9/rkr[0x59fe7e]
/snap/riker/x9/rkr[0x46200d]
/lib/x86_64-linux-gnu/libc.so.6(__libc_start_main+0xf3)[0x7ffff7a830b3]
/snap/riker/x9/rkr[0x47514b]
/snap/riker/x9/rkr[0x497f6e]
/snap/riker/x9/rkr[0x47cb81]
/snap/riker/x9/rkr[0x40d53e]
/snap/riker/x9/rkr[0x473d07]
Warning: unable to launch addr2line. Using backtrace_symbols as a fallback.
/lib/x86_64-linux-gnu/libc.so.6(__libc_start_main+0xf3)[0x7ffff7a830b3]
/snap/riker/x9/rkr[0x40d53e]
Warning: unable to launch addr2line. Using backtrace_symbols as a fallback.
```
This seems to relate to the fact that a Snap is strictly confined and thus access to external file systems are very limited and restricted. A possible solution might to be add more interfaces into the plugin part of the apps, such as `system-files`, `personal-files`, etc. The current `snapcraft.yaml` file includes partly of this potential fix, however, more researches need to be done on this suggestion.

#### mksquashfs

*If you are new to squashfs, you can check this out [Creating and using squashed file systems](https://tldp.org/HOWTO/SquashFS-HOWTO/creatingandusing.html)*

`mksquashfs` is a Linux tool which we use to compress a snap-structurted directory into a SquashFS filesystem - an isolated read-only file system of a standard snap. Rather than letting the tool to build, stage, and prime the program with specific install target in a VM before snapping everything into a `.snap` as in the `snapcraft`, our `mksquashfs` approach takes advantage of the fact that Riker has to be built before generating depedency list. Therefore, we can skip the first few steps of rebuilding Riker and just need to copying certain executable files and packages into a `squashfs-root` folder which is constructed to align strictly with the structure of a snap before compression:
```
riker
└── squashfs-root/
   └── meta/
       └── snapcraft.yaml
   └── snap/
       └── command-chain/
           └── snapcraft-runner
   └── rkr
   └── rkr-launch
   └── rkr-inject.so
   └── etc/
       └── ld.so.cache
   └── usr/
       └── lib/
           └── locale
              └── locale-archive
```
The construction of the file system is done by running `build-snap.sh`. Then we construct the `snap-craft.yaml` so as to be recognized by `snapd` later during the download and installation process. However, the current problem of this project is when we install the `riker.snap` and try running it, we got:
```
cannot snap-exec: cannot exec "/snap/riker/x10/snap/command-chain/snapcraft-runner": permission denied
```
Again, this problem could potentiall be fixed by allowing the snap more permissions and less strictly confined.

### 4. Describe one or two examples that demonstrate the new functionality you added. 

When you are finished with the production process, you should first build the program with `../riker/rkr`. This will let riker cache all the dependencies and store them as artifact. To get the list of all dependencies, `.rkr-deps`, run `../riker/rkr generate-deps`. In order to generate a more containerized version of program, explore your options `../riker/rkr generate-deps --help` and follow the instructions:
```
$ ./rkr generate-deps --help
Generate .rkr-deps that contains all the necessary dependencies
Usage: ./rkr generate-deps [OPTIONS]

Options:
  -h,--help                   Print this help message and exit
  -c,--container              Generate a Dockerfile in the .devcontainer folder that is compatible with vscode
  -q,--snap-squashfs          Generate a Snap using squashfs
  -s,--snap-snapcraft         Generate a Snap using snapcraft
```
If you already have `snapcraft` installed, simply run `../riker/rkr generate-deps -s`. This will generate a `snapcraft.yaml` file, build Riker, and compress everything into `riker_0.1_amd64.snap` (default naming by snapcraft). This approach provides you with more flexibility, as you can change metadata of Riker and rerun `$ snapcraft` on the command line to rebuild the snap.

If you wish not to install additional tool, you can resort to `mksquashfs` approach by running `../riker/rkr generate-deps -q`. This will create a `riker.snap`. However, the outcome of this approach is still under development and testing.

### 5. Describe next steps for this task, if there are any.

Currently the Snap generation is not fully complete and functional in both directions. Problems and potential solutions for both approaches are mentioned in the previous section. This would probably require a deeper understanding of Snap and its kernel mechanism, as well as experience building snap in different ways. Additionally, we need to resolve the problem of incompatibility for certain dependencies in our package list.

If we managed to solve all these problems, one very potential next step would be to register our snap name & version, test its stability, and finally release its stable channel to the Snap store.
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

## Prerequisites

`dodo` currently only runs on Linux.

In order to build `dodo`, you will need to install `capnproto` and `libcapnp-dev` libraries.  On Ubuntu:

```
$ sudo apt install capnproto libcapnp-dev
```

`dodo-build` outputs dependency graphs in GraphViz format.  To install GraphViz on Ubuntu:

```
$ sudo apt install graphviz
```

## Building

To build `dodo`, run
```
$ make
```

## Running `dodo`

`dodo` does not distinguish between building your project for the first time and rebuilding.  However, since developers need to do some initial setup when creating a new `dodo` project, we describe the two uses separately here.

### For the impatient

1. Create a `Dodofile`.
2. Run `dodo`.

### First build

We will demonstrate using `dodo` by way of example.  Here is a simple C program.

```
#include <stdio.h>

int main() {
  printf("Hello world!\n");
}
```

Create a `Dodofile` containing your build commands.  A `Dodofile` may be any executable file.  For example, it could be:

```
#!/bin/sh

gcc *.c
mv a.out helloworld
```

Be sure that your `Dodofile` is marked executable.  E.g.,

```
$ chmod +x Dodofile
```

Assuming that `dodo` is in your path, run

```
$ dodo
```

### Rebuilds

To rebuild, run
```
$ dodo
```

Notice that this is exactly the same as the first use.

If you aren't convinced that `dodo` is working, try

```
$ rm helloworld
$ dodo
```

## GraphViz output

`dodo` can be configured to output the build dependence graph using the `--visualize` flag.  The file will be called `out.dot`, which can be rendered to PDF using the GraphViz `dot` command, e.g.  
```
$ dodo --visualize
$ dot -Tpdf out.dot >out.pdf
```

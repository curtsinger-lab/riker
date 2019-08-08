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

To build, run
```
$ make
$ ./dodo "make -B"
```

To rebuild, run
```
$ ./dodo-build --unchanged src .
```

This will also output `out.dot`, which can be rendered to PDF using the GraphViz `dot` command, e.g.
```
$ dot -Tpdf out.dot >out.pdf
```

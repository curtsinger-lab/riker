Run the build with a buildfile that is just a compiled C program

Move to test directory
  $ cd $TESTDIR

Prepare for a the build
  $ rm -rf .dodo output
  $ gcc -o Dodofile c-Dodofile.c

Run the first build
  $ $DODO --show
  dodo-launch
  Dodofile

Check the output
  $ cat output
  Hello from C

Run a rebuild, which should do nothing
  $ $DODO --show

Now make the build file unreadable as well
  $ chmod a-rx Dodofile

Run a build, which should fail
  $ $DODO --show
  dodo-launch
  Unable to launch build file Dodofile.
    Write build steps in a file named `Dodofile`.
    This file must be either directly executable, or runnable with `/bin/sh`.

Make the Dodofile readable and executable again
  $ chmod u+rx Dodofile

Run a build
  $ $DODO --show
  dodo-launch
  Dodofile

Check the output
  $ cat output
  Hello from C

Run a rebuild, which should do nothing
  $ $DODO --show

Clean up
  $ rm -rf .dodo output Dodofile

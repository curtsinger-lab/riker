Run the build with a buildfile that is just a compiled C program

Move to test directory
  $ cd $TESTDIR

Prepare for a the build
  $ rm -rf .rkr output Rikerfile
  $ gcc -o Rikerfile c-Rikerfile.c

Run the first build
  $ $RKR --show
  rkr-launch
  Rikerfile

Check the output
  $ cat output
  Hello from C

Run a rebuild, which should do nothing
  $ $RKR --show

Now make the build file unreadable as well
  $ chmod a-rx Rikerfile

Run a build, which should fail
  $ $RKR --show
  rkr-launch
  Unable to launch build file Rikerfile.
    Write build steps in a file named `Rikerfile`.
    This file must be either directly executable, or runnable with `/bin/sh`.

Make the Rikerfile readable and executable again
  $ chmod u+rx Rikerfile

Run a build
  $ $RKR --show
  rkr-launch
  Rikerfile

Check the output
  $ cat output
  Hello from C

Run a rebuild, which should do nothing
  $ $RKR --show

Clean up
  $ rm -rf .rkr output Rikerfile

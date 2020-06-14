Run the build with a buildfile that runs with /bin/sh

Move to test directory
  $ cd $TESTDIR

Prepare for a the build
  $ rm -rf .dodo output
  $ cp sh-Dodofile Dodofile
  $ chmod u+x Dodofile

Run the first build
  $ $DODO --show
  dodo-launch
  Dodofile

Check the output
  $ cat output
  Hello

Run a rebuild, which should do nothing
  $ $DODO --show

Make the build file non-executable
  $ chmod a-x Dodofile

Run a build
  $ $DODO --show
  dodo-launch
  /bin/sh Dodofile

Check the output
  $ cat output
  Hello

Run a rebuild, which should do nothing
  $ $DODO --show

Now make the build file unreadable as well
  $ chmod a-r Dodofile

Run a build, which should fail
  $ $DODO --show
  dodo-launch
  Unable to access Dodofile.
    This file must be directly executable or runnable with /bin/sh.

Make the Dodofile readable and executable again
  $ chmod u+rx Dodofile

Run a build
  $ $DODO --show
  dodo-launch
  Dodofile

Check the output
  $ cat output
  Hello

Run a rebuild, which should do nothing
  $ $DODO --show

Clean up
  $ rm -rf .dodo output Dodofile

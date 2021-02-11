Run the build with a buildfile that runs with /bin/sh

Move to test directory
  $ cd $TESTDIR

Prepare for a the build
  $ rm -rf .dodo output Rikerfile
  $ cp sh-Rikerfile Rikerfile
  $ chmod u+x Rikerfile

Run the first build
  $ $DODO --show
  dodo-launch
  Rikerfile

Check the output
  $ cat output
  Hello

Run a rebuild, which should do nothing
  $ $DODO --show

Make the build file non-executable
  $ chmod a-x Rikerfile

Run a build
  $ $DODO --show
  dodo-launch
  sh Rikerfile

Check the output
  $ cat output
  Hello

Run a rebuild, which should do nothing
  $ $DODO --show

Now make the build file unreadable as well
  $ chmod a-r Rikerfile

Run a build, which should fail
  $ $DODO --show
  dodo-launch
  Unable to launch build file Rikerfile.
    Write build steps in a file named `Rikerfile`.
    This file must be either directly executable, or runnable with `/bin/sh`.

Make the Rikerfile readable and executable again
  $ chmod u+rx Rikerfile

Run a build
  $ $DODO --show
  dodo-launch
  Rikerfile

Check the output
  $ cat output
  Hello

Run a rebuild, which should do nothing
  $ $DODO --show

Clean up
  $ rm -rf .dodo output Rikerfile

Test using environment variables that change outside a build. 

SKIP! This test doesn't work because riker doesn't rerun commands in response to environment variable changes.
  $ exit 80

Move to test directory
  $ cd $TESTDIR

Copy in the add Rikerfile and make sure it is executable
  $ cp add-Rikerfile Rikerfile
  $ chmod u+x Rikerfile

Prepare for a clean run
  $ rm -rf .rkr output
  $ echo "hello" > input
  $ gcc -o do_work do_work.c

Run the first build
  $ $RKR --show
  rkr-launch
  Rikerfile
  ./do_work

Check the output
  $ cat output
  hello
  goodbye

Change the environment variable
  $ export EXTRA_MESSAGE="goodnight"

Now rerun the build
  $ $RKR --show
  ./do_work

Check output file
  $ cat output
  hello
  goodnight

Clean up
  $ rm -rf .rkr output do_work Rikerfile

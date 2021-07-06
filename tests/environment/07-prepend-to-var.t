Test using environment variables that change during a build. This time we prepend the environment variable rather than changing the whole variable

Move to test directory
  $ cd $TESTDIR

Copy in the add Rikerfile and make sure it is executable
  $ cp prepend-Rikerfile Rikerfile
  $ chmod u+x Rikerfile

Prepare for a clean run
  $ rm -rf .rkr output
  $ echo "hello" > input
  $ gcc -o do_work do_work.c

Run the first build
  $ export EXTRA_EXTRA_MESSAGE=goodnight
  $ $RKR --show
  rkr-launch
  Rikerfile
  ./do_work

Check the output
  $ cat output
  hello
  goodmorning:goodnight

Change the input file
  $ export EXTRA_EXTRA_MESSAGE=goodday
  $ echo "howdy" > input

Now rerun the build
  $ $RKR --show
  ./do_work

Check output file
  $ cat output
  howdy
  goodmorning:goodday

Clean up 
  $ rm -rf .rkr output do_work Rikerfile
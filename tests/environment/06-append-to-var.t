Test using environment variables that change during a build. This time we append an environment variable rather than changing the whole variable

Move to test directory
  $ cd $TESTDIR

Copy in the add Rikerfile and make sure it is executable
  $ cp append-Rikerfile Rikerfile
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
  goodnight:goodmorning

Change the input file
  $ export EXTRA_EXTRA_MESSAGE=goodday
  $ echo "howdy" > input

Now rerun the build
  $ $RKR --show
  ./do_work

Check output file
  $ cat output
  howdy
  goodday:goodmorning

Clean up 
  $ rm -rf .rkr output do_work Rikerfile

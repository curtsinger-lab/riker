Test using environment variables that change during a build. This time we change the input file and add an extra variable outside the build.

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

Change the input file
  $ echo "howdy" > input
  $ export EXTRA_EXTRA_MESSAGE=goodnight

Now rerun the build
  $ $RKR --show
  ./do_work

Check output file
  $ cat output
  howdy
  goodbye
  goodnight

Clean up 
  $ rm -rf .rkr output do_work Rikerfile

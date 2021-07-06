Test using environment variables that change during a build. This time we change the input file and delete an environment variable. 

Move to test directory
  $ cd $TESTDIR

Copy in the delete Rikerfile and make sure it is executable
  $ cp delete-Rikerfile Rikerfile
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

Change the input file
  $ echo "howdy" > input

Now rerun the build
  $ export EXTRA_EXTRA_MESSAGE=goodnight
  $ $RKR --show
  ./do_work

Check output file
  $ cat output
  howdy

Clean up
  $ rm -rf .rkr output do_work Rikerfile

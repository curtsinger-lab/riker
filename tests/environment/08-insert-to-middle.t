Test using environment variables that change during a build. This time we insert an environment variable into the center of the tokens.
This cannot be handled with appends and prepends, so it should be replaced instead. 

Move to test directory
  $ cd $TESTDIR

Copy in the add Rikerfile and make sure it is executable
  $ cp insert-Rikerfile Rikerfile
  $ chmod u+x Rikerfile

Prepare for a clean run
  $ rm -rf .rkr output
  $ echo "hello" > input
  $ gcc -o do_work do_work.c

Run the first build
  $ export EXTRA_EXTRA_MESSAGE=goodmorning:goodnight
  $ $RKR --show
  rkr-launch
  Rikerfile
  ./do_work

Check the output
  $ cat output
  hello
  goodmorning:goodafternoon:goodnight

Change the input file
  $ export EXTRA_EXTRA_MESSAGE=goodday
  $ echo "howdy" > input

Now rerun the build
  $ $RKR --show
  ./do_work

Check output file
  $ cat output
  howdy
  goodmorning:goodafternoon:goodnight

Clean up 
  $ rm -rf .rkr output do_work Rikerfile
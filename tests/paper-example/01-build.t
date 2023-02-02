Run an initial build and a rebuild with no changes

Move to test directory
  $ cd $TESTDIR

Prepare for a clean run. Create an empty output file for now, so rebuilding works
  $ rm -rf .rkr program z.*
  $ cp init/main.c main.c

Run the first build
  $ rkr --show-full --no-wrapper
  rkr-launch
  Rikerfile
  gcc -Wall -o program main.c x.c y.c
  **cc1 * main* * (glob)
  **as * (glob)
  **cc1 * x* * (glob)
  **as * (glob)
  **cc1 * y* * (glob)
  **as * (glob)
  **collect2 * program * (glob)
  **ld * program * (glob)

Add some files to the current directory and alter main.c
  $ cp change/* .

Run a rebuild
  $ rkr --show-full --no-wrapper
  Rikerfile
  gcc -Wall -o program main.c x.c y.c z.c
  **cc1 * main* * (glob)
  **as * (glob)
  **cc1 * z* * (glob)
  **as * (glob)
  **collect2 * program * (glob)
  **ld * program * (glob)

Clean up
  $ rm -rf .rkr program z.*
  $ cp init/main.c main.c

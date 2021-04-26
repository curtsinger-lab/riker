Run an initial build and a rebuild with no changes

Move to test directory
  $ cd $TESTDIR

Prepare for a clean run. Create an empty output file for now, so rebuilding works
  $ rm -rf .rkr program z.*
  $ cp init/main.c main.c

Run the first build
  $ $RKR --show
  rkr-launch
  sh Rikerfile
  gcc -Wall -o program main.c x.c y.c
  [^ ]*cc1 .* (re)
  [^ ]*as .* (re)
  [^ ]*cc1 .* (re)
  [^ ]*as .* (re)
  [^ ]*cc1 .* (re)
  [^ ]*as .* (re)
  [^ ]*collect2 .* (re)
  [^ ]*ld .* (re)

#Run a rebuild
#  $ $RKR --show

#Check the output again
#  $ cat output
#  Hello

Clean up
  $ rm -rf .rkr program z.*
  $ cp init/main.c main.c
  $ rm output

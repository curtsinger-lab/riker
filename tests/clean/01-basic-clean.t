This test cleans the .rkr directory after the build and verifies that a rebuild will do a fresh build 

Move to test directory 
  $ cd $TESTDIR 

Clean up any leftover state 
  $ rm -rf .rkr hello 

Make sure Rikerfile is executable 
  $ chmod u+x Rikerfile 

Run the build 
  $ $RKR --show 
  rkr-launch
  Rikerfile
  gcc -o hello hello.c
  [^ ]*cc1 .* (re)
  [^ ]*as .* (re)
  [^ ]*collect2 .* (re)
  [^ ]*ld .* (re)

Run the hello executable
  $ ./hello
  Hello world

Clean the .rkr directory 
  $ $RKR clean 
  .rkr removed

Run the rebuild, which should do a fresh build 
  $ $RKR --show 
  rkr-launch
  Rikerfile
  gcc -o hello hello.c
  [^ ]*cc1 .* (re)
  [^ ]*as .* (re)
  [^ ]*collect2 .* (re)
  [^ ]*ld .* (re)

Clean up 
  $ rm -rf .rkr hello
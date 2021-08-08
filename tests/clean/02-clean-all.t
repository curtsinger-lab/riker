This test cleans all the files created during the build except .rkr directory. 

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

Clean all files created during the build except .rkr 
  $ $RKR clean -a
  [^ ]*Removing .* (re)

Make sure the executable is removed 
  $ ls hello 
  ls: cannot access 'hello': No such file or directory
  [2]

Run the rebuild, which should do nothing
  $ $RKR --show 

Run the hello executable
  $ ./hello
  Hello world

Clean up 
  $ rm -rf .rkr hello
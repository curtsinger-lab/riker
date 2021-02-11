Run a build and verify that no rebuild is nessecary

Move to test directory
  $ cd $TESTDIR

Prepare for a clean run
  $ rm -rf .rkr *.s hello.c

Prepare for the first run
This step is necessary because gcc stores input file names in assembler output
  $ cp hello.no_ws.c hello.c

Run the first build
  $ $RKR --show
  rkr-launch
  Rikerfile
  cc1 .* (re)
  cp hello.s output.s

Save 
  $ cp output.s output.no_ws.s

Prepare for the second run
  $ cp hello.ws.c hello.c

Run a rebuild
  $ $RKR --show
  cc1 .* (re)

The two files should be the same
  $ diff output.no_ws.s output.s

Clean up
  $ rm -rf .rkr *.s hello.c

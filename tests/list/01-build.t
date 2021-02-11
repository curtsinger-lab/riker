Run an initial build

Move to test directory
  $ cd $TESTDIR

Prepare for a clean run
  $ rm -rf .rkr output *.txt
  $ echo hello > hello.txt
  $ echo world > world.txt

Run the first build
  $ $RKR --show
  rkr-launch
  Rikerfile
  cat hello.txt world.txt

Check the output
  $ cat output
  hello
  world

Run a rebuild, which shoudl do nothing
  $ $RKR --show

Check the output
  $ cat output
  hello
  world

Clean up
  $ rm -rf .rkr output *.txt

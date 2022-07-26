Run a build that appends to a file after possibly removing it with `rm -f`

Move to test directory
  $ cd $TESTDIR

Prepare for a clean run
  $ rm -rf .rkr output.txt
  $ echo "echo abc" > make-output.sh

Run the first build
  $ rkr --show
  rkr-launch
  Rikerfile
  rm -f output.txt
  sh make-output.sh

Check the output
  $ cat output.txt
  abc

Run a rebuild
  $ rkr --show

Check the output
  $ cat output.txt
  abc

SKIP! This test is currently broken. See issue #39: <https://github.com/curtsinger-lab/riker/issues/39>
  $ exit 80

Now change make-output.sh
  $ echo "echo abcd" > make-output.sh

Update the build
  $ rkr --show
  sh make-output.sh

Check the output
  $ cat output.txt
  abcd

Run a rebuild
  $ rkr --show

Check the output one last time
  $ cat output.txt
  abcd

Clean up
  $ rm -rf .rkr output.txt
  $ echo "echo abc" > make-output.sh

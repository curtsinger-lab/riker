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

SKIP! This test is currently broken. The reason is subtle, and connected with post-build checks.
Updating make-output.sh forces that command to rerun, but instead of starting with an empty output.txt it will append to the existing file (which is inconsistent with a full build). The problem is that `rm -f output.txt` never re-executes. That happens because output.txt did not exist on the first run of `rm -f output.txt`, but does exist during post-build checks. That allows rkr to skip `rm` on all future builds.
Removing post-build checks entirely would fix the issue. Builds with gcc would re-run the top-level compiler invocation on the second build, but we'd skip all the actual compilation work. That doesn't seem ideal, but it may be acceptable.
Another option is to allow alternative outcomes for predicates in commands only if the cause of that change is the command itself; for example, `gcc -o program ...` doesn't have to care if the program exists if it is `gcc` itself that creates the file. That would solve the issue because `rm -f output.txt` doesn't create output.txt---that happens later.
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

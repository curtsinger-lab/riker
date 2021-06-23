Run an initial build

Move to test directory
  $ cd $TESTDIR

Prepare for a clean run
  $ rm -rf .rkr foo a output
  $ echo "Hello" > input

Run the first build
  $ rkr --show
  rkr-launch
  sh Rikerfile
  mkdir foo
  mkdir foo/bar
  mkdir foo/bar/baz
  mkdir -p a/b/c
  ln -s ../../../a/b/c/out foo/bar/baz/win
  cat ../../../input
  mv a/b/c/out output
  rm -rf foo a

Check the output
  $ cat output
  Hello

Change the input file
  $ echo "Goodbye" > input

Run a rebuild
  $ rkr --show
  cat ../../../input

Check the output
  $ cat output
  Goodbye

Run a rebuild
  $ rkr --show

Clean up
  $ rm -rf .rkr output foo a
  $ echo "Hello" > input

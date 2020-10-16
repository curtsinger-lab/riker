Run a rebuild after changing each of the inputs

Move to test directory
  $ cd $TESTDIR

Prepare for a clean run
  $ rm -rf .dodo myfile
  $ echo -n "hello" > inputA
  $ echo " world" > inputB

Run the first build
  $ $DODO --show
  dodo-launch
  Dodofile
  ./A
  cat inputA
  ./B
  cat inputB

Check the output
  $ cat myfile
  hello world

Run a rebuild
  $ $DODO --show

Check the output
  $ cat myfile
  hello world

Change inputA
  $ echo -n "goodbye" > inputA

Run a rebuild
  $ $DODO --show
  cat inputA
  cat inputB

Check the output
  $ cat myfile
  goodbye world

Run another rebuild, which should do nothing now
  $ $DODO --show

Change inputB
  $ echo " frodo" > inputB

Run a rebuild
  $ $DODO --show
  cat inputA
  cat inputB

Check the output
  $ cat myfile
  goodbye frodo

Run another rebuild, which should do nothing
  $ $DODO --show

Check the output again
  $ cat myfile
  goodbye frodo

Clean up
  $ rm -rf .dodo myfile
  $ echo -n "hello" > inputA
  $ echo " world" > inputB

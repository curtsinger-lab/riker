The `date` command takes no inputs and is nondeterministic.
Therefore, if `dodo` already has an output, it should only ever
launch the command once, since all outputs are weakly equivalent.

Move to test directory
  $ cd $TESTDIR

Clean up any leftover state
  $ rm -rf .dodo A

Run the build
  $ $DODO --show
  dodo-launch
  sh Dodofile
  date

Run the build again (nothing should happen)
  $ $DODO --show
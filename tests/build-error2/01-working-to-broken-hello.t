Run a build that succeeds, then rebuild after introducing an error in hello.c.

Move to test directory
  $ cd $TESTDIR

Prepare for a clean run
  $ rm -rf .rkr hello
  $ cp versions/hello-working.c hello.c
  $ cp versions/world-working.c world.c

Run the first build
  $ $RKR --show
  rkr-launch
  Rikerfile
  gcc -o hello hello.c world.c
  cc1 * (glob)
  as * (glob)
  cc1 * (glob)
  as * (glob)
  collect2 * (glob)
  ld * (glob)

Check the output
  $ ./hello
  Hello world.

Run a rebuild, which should do nothing
  $ $RKR --show

Stage in a broken version of the hello.c source file
  $ cp versions/hello-broken.c hello.c

Run a rebuild. This will rerun cc1, which fails. That forces gcc to rerun. The gcc command fails, which forces a rerun of Rikerfile as well.

This test used to fail because of a command matching issue. The gcc command does not run `as` to process the output from the failed `cc1` command, but when we traced the second `as` launch (the one that processes output from `cc1 world.c`) we used to match it to that command.

The test has been fixed by preserving matching order. Once we've matched the second `cc1` command from `gcc`, we can no longer go back and match against the first `as` command. It's not clear how this fix could be extended to work with global command matching. A better approach may be to match commands speculatively; we could maintain a set of commands that might be matches, then remove them from that set if emulating them results in any failed predicates.

Or as an alternative: do we get what we want if we prefer matches against commands marked Emulate? That's sort of like the speculative matching approach, except we rely on dependency information from the previous build iteration.
  $ $RKR --show
  cc1 * (glob)
  hello.c: In function 'main':
  hello.c:6:19: error: expected ';' before 'print_world'
      6 |   printf("Hello ")
        |                   ^
        |                   ;
      7 |   print_world();
        |   ~~~~~~~~~~~    * (glob)
  gcc -o hello hello.c world.c
  Rikerfile

Run a rebuild, which should do nothing
  $ $RKR --show

Clean up
  $ rm -rf .rkr
  $ rm hello
  $ cp versions/hello-working.c hello.c
  $ cp versions/world-working.c world.c

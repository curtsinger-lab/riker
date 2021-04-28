Test rebuilds with a Rikerfile that uses wildcards when files are removed (one and two at a time)

Move to test directory
  $ cd $TESTDIR

Prepare for a clean run
  $ rm -rf .rkr program
  $ cp versions/a-original.c a.c
  $ cp versions/b-original.c b.c
  $ cp versions/c-original.c c.c

Run the first build
  $ $RKR --show
  rkr-launch
  Rikerfile
  gcc -o program a.c b.c c.c main.c
  cc1 * (glob)
  as * (glob)
  cc1 * (glob)
  as * (glob)
  cc1 * (glob)
  as * (glob)
  cc1 * (glob)
  as * (glob)
  collect2 * (glob)
  ld * (glob)

Check the output
  $ ./program
  Hello from main.
  Hello from a.
  Hello from b.
  Hello from c.

Remove c.c
  $ rm c.c

Run a rebuild. This should rerun Rikerfile (because "." is changed), which launches a new gcc. We should match all of the cc1 and as commands and just rerun collect2 and ld. This test currently fails because gcc is a new command (with new arguments) so it has no children to match against. Matching against a global list of commands will fix this
  $ $RKR --show
  Rikerfile
  gcc -o program a.c b.c main.c
  collect2 * (glob)
  ld * (glob)

Check the output
  $ ./program
  Hello from main.
  Hello from a.
  Hello from b.

Rebuilding should do nothing
  $ $RKR --show

Remove a.c and b.c
  $ rm a.c b.c

Run a rebuild. This should rerun Rikerfile again and relink without rerunning any compilation steps. This test should pass once commands are matched against a global list.
  $ $RKR --show
  Rikerfile
  gcc -o program main.c
  collect2 * (glob)
  ld * (glob)

Check the output
  $ ./program
  Hello from main.

Rebuilding should do nothing
  $ $RKR --show

Clean up
  $ rm -rf .rkr
  $ rm program

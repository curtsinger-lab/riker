Test rebuilds with a Rikerfile that uses wildcards when files are added (one and two at a time).

Move to test directory
  $ cd $TESTDIR

Prepare for a clean run
  $ rm -rf .rkr program
  $ rm -f a.c b.c c.c

Run the first build
  $ rkr --show --no-wrapper
  rkr-launch
  Rikerfile
  gcc -o program main.c
  cc1 * (glob)
  as * (glob)
  collect2 * (glob)
  ld * (glob)

Check the output
  $ ./program
  Hello from main.

Add in a.c
  $ cp versions/a-original.c a.c

Run a rebuild. This should rerun Rikerfile (because "." is changed), which launches a new gcc. We should match the cc1 and as commands for main.c and just run the new commands for a.c. The gcc command is new in this case, so this test will fail until we match against a global list of commands.
  $ rkr --show --no-wrapper
  Rikerfile
  gcc -o program a.c main.c
  cc1 * (glob)
  as * (glob)
  collect2 * (glob)
  ld * (glob)

Check the output
  $ ./program
  Hello from main.
  Hello from a.

Rebuilding should do nothing
  $ rkr --show

Add in b.c and c.c
  $ cp versions/b-original.c b.c
  $ cp versions/c-original.c c.c

Run a rebuild. This should rerun Rikerfile again, then the cc1 and as command for b.c and c.c only (not main.c or a.c)
  $ rkr --show --no-wrapper
  Rikerfile
  gcc -o program a.c b.c c.c main.c
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

Rebuilding should do nothing
  $ rkr --show

Clean up
  $ rm -rf .rkr
  $ rm program a.c b.c c.c

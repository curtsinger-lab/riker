Test rebuilds with a Rikerfile that uses wildcards when files are modified.

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

Modify a.c
  $ cp versions/a-modified.c a.c

Run a rebuild. This should only recompile a.c and relink
  $ $RKR --show
  cc1 * (glob)
  as * (glob)
  ld * (glob)

Check the output
  $ ./program
  Hello from main.
  Goodbye from a.
  Hello from b.
  Hello from c.

Rebuilding should do nothing
  $ $RKR --show

Now modify b.c and c.c
  $ cp versions/b-modified.c b.c
  $ cp versions/c-modified.c c.c

Run a rebuild. This should recompile b.c and c.c, then relink. The cc1 commands both run first because they are are marked for rerun on the first iteration.
  $ $RKR --show
  cc1 * (glob)
  cc1 * (glob)
  as * (glob)
  as * (glob)
  ld * (glob)

Check the output
  $ ./program
  Hello from main.
  Goodbye from a.
  Goodbye from b.
  Goodbye from c.

Rebuilding should do nothing
  $ $RKR --show

Clean up
  $ rm -rf .rkr
  $ rm program a.c b.c c.c

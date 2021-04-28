Test rebuilds with a Rikerfile that uses wildcards when files are added and modified in combination.

Move to test directory
  $ cd $TESTDIR

Prepare for a clean run
  $ rm -rf .rkr program
  $ rm -f a.c b.c c.c

Run the first build
  $ $RKR --show
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

Add a.c
  $ cp versions/a-original.c a.c

Run a rebuild. This should reinvoke Rikerfile and gcc to capture the new file.
  $ $RKR --show
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
  $ $RKR --show

Now modify a.c and add b.c and c.c
  $ cp versions/a-modified.c a.c
  $ cp versions/b-original.c b.c
  $ cp versions/c-original.c c.c

Run a rebuild. This should rerun Rikerfile and gcc to capture the new files, recompile a.c, and add cc1 and as invokations for b.c and c.c
  $ $RKR --show
  Rikerfile
  gcc -o program a.c b.c c.c main.c
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
  Goodbye from a.
  Hello from b.
  Hello from c.

Rebuilding should do nothing
  $ $RKR --show

Now modify b.c and c.c
  $ cp versions/b-modified.c b.c
  $ cp versions/c-modified.c c.c

Run a rebuild. This should just recompile b.c and c.c, then relink
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

Test rebuilds with a Rikerfile that uses wildcards when files are added and modified in combination.

Move to test directory
  $ cd $TESTDIR

Prepare for a clean run
  $ rm -rf .rkr program
  $ cp versions/a-original.c a.c
  $ cp versions/b-original.c b.c
  $ cp versions/c-original.c c.c

Run the first build
  $ rkr --show --no-wrapper
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

Remove a.c and modify b.c
  $ rm a.c
  $ cp versions/b-modified.c b.c

Run a rebuild. This should reinvoke Rikerfile and gcc to capture the change in directory contents, then rebuild b.c and relink
  $ rkr --show --no-wrapper
  Rikerfile
  gcc -o program b.c c.c main.c
  cc1 * (glob)
  as * (glob)
  collect2 * (glob)
  ld * (glob)

Check the output
  $ ./program
  Hello from main.
  Goodbye from b.
  Hello from c.

Rebuilding should do nothing
  $ rkr --show

Now modify both b.c and c.c
  $ cp versions/b-original.c b.c
  $ cp versions/c-modified.c c.c

Run a rebuild. This should recompile b.c and c.c, then relink
  $ rkr --show --no-wrapper
  cc1 * (glob)
  cc1 * (glob)
  as * (glob)
  as * (glob)
  ld * (glob)

Check the output
  $ ./program
  Hello from main.
  Hello from b.
  Goodbye from c.

Rebuilding should do nothing
  $ rkr --show

Now remove c.c and modify b.c again
  $ cp versions/b-modified.c b.c
  $ rm c.c

Run a rebuild. This should rerun Rikerfile and gcc to capture the change in wildcard match. Then recompile b.c and relink.
  $ rkr --show --no-wrapper
  Rikerfile
  gcc -o program b.c main.c
  cc1 * (glob)
  as * (glob)
  collect2 * (glob)
  ld * (glob)

Check the output
  $ ./program
  Hello from main.
  Goodbye from b.

Rebuilding should do nothing
  $ rkr --show

Clean up
  $ rm -rf .rkr
  $ rm -f program a.c b.c c.c

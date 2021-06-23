Run a build that succeeds, then rebuild after introducing an error in hello.c.

Move to test directory
  $ cd $TESTDIR

Prepare for a clean run
  $ rm -rf .rkr hello
  $ cp versions/hello-working.c hello.c
  $ cp versions/world-working.c world.c

Run the first build
  $ rkr --show
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
  $ rkr --show

Stage in a broken version of the hello.c source file
  $ cp versions/hello-broken.c hello.c

Run a rebuild. This will rerun cc1, which fails. That forces gcc to rerun. The gcc command fails, which forces a rerun of Rikerfile as well.
  $ rkr --show
  cc1 * (glob)
  hello.c: In function 'main':
  hello.c:6:19: error: expected ';' before 'print_world'
      6 |   printf("Hello ")
        |                   ^
        |                   ;
      7 |   print_world();
        |   ~~~~~~~~~~~      
  gcc -o hello hello.c world.c
  Rikerfile

Run a rebuild, which should do nothing
  $ rkr --show

Clean up
  $ rm -rf .rkr
  $ rm hello
  $ cp versions/hello-working.c hello.c
  $ cp versions/world-working.c world.c

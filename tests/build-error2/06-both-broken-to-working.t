Run a build with errors in both source files, then fix the errors and rebuild.

Move to test directory
  $ cd $TESTDIR

Prepare for a clean run
  $ rm -rf .rkr hello

Stage in a broken versions of both source files
  $ cp versions/hello-broken.c hello.c
  $ cp versions/world-broken.c world.c

Run a rebuild. This will rerun cc1, which fails. That forces gcc to rerun. The gcc command fails, which forces a rerun of Rikerfile as well.
  $ $RKR --show
  rkr-launch
  Rikerfile
  gcc -o hello hello.c world.c
  cc1 * (glob)
  hello.c: In function 'main':
  hello.c:6:19: error: expected ';' before 'print_world'
      6 |   printf("Hello ")
        |                   ^
        |                   ;
      7 |   print_world();
        |   ~~~~~~~~~~~      
  cc1 * (glob)
  world.c: In function 'print_world':
  world.c:4:21: error: expected ';' before '}' token
      4 |   printf("world.\n")
        |                     ^
        |                     ;
      5 | }
        | ~                    

Run a rebuild, which should do nothing
  $ $RKR --show

Stage in a working versions of both source files
  $ cp versions/hello-working.c hello.c
  $ cp versions/world-working.c world.c

Run the first build
  $ $RKR --show
  cc1 * (glob)
  cc1 * (glob)
  gcc -o hello hello.c world.c
  as * (glob)
  as * (glob)
  collect2 * (glob)
  ld * (glob)
  Rikerfile

Check the output
  $ ./hello
  Hello world.

Run a rebuild, which should do nothing
  $ $RKR --show

Clean up
  $ rm -rf .rkr
  $ rm hello
  $ cp versions/hello-working.c hello.c
  $ cp versions/world-working.c world.c

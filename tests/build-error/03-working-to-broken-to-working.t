Move to test directory
  $ cd $TESTDIR

Prepare for a clean run
  $ rm -rf .rkr hello
  $ cp versions/hello-original.c hello.c

Run the first build
  $ $RKR --show
  rkr-launch
  Rikerfile
  gcc -o hello hello.c
  cc1 * (glob)
  as * (glob)
  collect2 * (glob)
  ld * (glob)

Check the output
  $ ./hello
  Hello world.

Stage in a broken version of the source file
  $ cp versions/hello-broken.c hello.c

Run a rebuild. This will rerun cc1, which fails. That forces gcc to rerun.
The gcc command fails, which forces a rerun of Rikerfile as well.
  $ $RKR --show
  cc1 * (glob)
  hello.c: In function 'main':
  hello.c:4:27: error: expected ';' before 'return'
      4 |   printf("Hello world.\n")
        |                           ^
        |                           ;
      5 |   return 0;
        |   ~~~~~~                   
  gcc -o hello hello.c
  Rikerfile

Stage in a working version of the source file
  $ cp versions/hello-original.c hello.c

Run a rebuild. This will rerun cc1, which now succeeds. That forces gcc to rerun.
The gcc command now succeeds, which forces a rerun of Rikerfile as well.
  $ $RKR --show
  cc1 * (glob)
  gcc -o hello hello.c
  as * (glob)
  collect2 * (glob)
  ld * (glob)
  Rikerfile

Run the target
  $ ./hello
  Hello world.

Run a rebuild, which should do nothing
  $ $RKR --show

Clean up
  $ rm -rf .rkr
  $ rm hello
  $ cp versions/hello-original.c hello.c

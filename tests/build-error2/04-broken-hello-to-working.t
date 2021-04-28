Run a build that starts with a broken version of hello.c, then a rebuild with a corrected file.

Move to test directory
  $ cd $TESTDIR

Prepare for a clean run
  $ rm -rf .rkr hello
  $ cp versions/hello-broken.c hello.c
  $ cp versions/world-working.c world.c

Run the first build
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
  as * (glob)

Run a rebuild, which should do nothing
  $ $RKR --show

Stage in a working version of the hello.c source file
  $ cp versions/hello-working.c hello.c

Run a rebuild. This will rerun `cc1 hello.c`. That will change its exit status, forcing a rerun of `gcc`, which launches a new `as` command, followed by `collect2` and `ld`. The `gcc` command will succeed, which in turn causes a rerun of Rikerfile
  $ $RKR --show
  cc1 * (glob)
  gcc -o hello hello.c world.c
  as * (glob)
  collect2 * (glob)
  ld * (glob)
  Rikerfile

Run the output program
  $ ./hello
  Hello world.

Run a rebuild, which should do nothing
  $ $RKR --show

Clean up
  $ rm -rf .rkr
  $ rm hello
  $ cp versions/hello-working.c hello.c
  $ cp versions/world-working.c world.c

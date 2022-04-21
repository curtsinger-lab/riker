Run a build that starts with a broken version of world.c, then a rebuild with a corrected file.

Move to test directory
  $ cd $TESTDIR

Prepare for a clean run
  $ rm -rf .rkr hello
  $ cp versions/hello-working.c hello.c
  $ cp versions/world-broken.c world.c

Run the first build
  $ rkr --show --no-wrapper
  rkr-launch
  Rikerfile
  gcc -o hello hello.c world.c
  cc1 * (glob)
  as * (glob)
  cc1 * (glob)
  world.c: In function 'print_world':
  world.c:4:21: error: expected ';' before '}' token
      4 |   printf("world.\n")
        |                     ^
        |                     ;
      5 | }
        | ~                    

Run a rebuild, which should do nothing
  $ rkr --show

Stage in a working version of the world.c source file
  $ cp versions/world-working.c world.c

Run a rebuild. This will rerun `cc1 hello.c`. That will change its exit status, forcing a rerun of `gcc`, which launches a new `as` command, followed by `collect2` and `ld`. The `gcc` command will succeed, which in turn causes a rerun of Rikerfile
  $ rkr --show --no-wrapper
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
  $ rkr --show

Clean up
  $ rm -rf .rkr
  $ rm hello
  $ cp versions/hello-working.c hello.c
  $ cp versions/world-working.c world.c

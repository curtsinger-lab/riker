Run a series of builds to cover a full cycle of changes:
1. Both correct
2. Broken hello.c
3. Both broken

A: both working
B: broken hello.c
C: broken world.c
D: both broken

Move to test directory
  $ cd $TESTDIR

Prepare for a clean run
  $ rm -rf .rkr hello

Stage in working versions of both source files
  $ cp versions/hello-working.c hello.c
  $ cp versions/world-working.c world.c

Run the first build
  $ rkr --show --no-wrapper
  rkr-launch
  Rikerfile
  gcc -o hello hello.c world.c
  cc1 * (glob)
  as * (glob)
  cc1 * (glob)
  as * (glob)
  collect2 * (glob)
  ld * (glob)

Run a rebuild, which should do nothing
  $ rkr --show

Check the output
  $ ./hello
  Hello world.

Both working -> Broken hello.c
  $ cp versions/hello-broken.c hello.c
  $ rkr --show --no-wrapper
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

Broken hello.c -> Broken world.c
  $ cp versions/hello-working.c hello.c
  $ cp versions/world-broken.c world.c
  $ rkr --show --no-wrapper
  cc1 * (glob)
  cc1 * (glob)
  world.c: In function 'print_world':
  world.c:4:21: error: expected ';' before '}' token
      4 |   printf("world.\n")
        |                     ^
        |                     ;
      5 | }
        | ~                    
  gcc -o hello hello.c world.c
  as * (glob)

Broken world.c -> Both broken
  $ cp versions/hello-broken.c hello.c
  $ rkr --show --no-wrapper
  cc1 * (glob)
  hello.c: In function 'main':
  hello.c:6:19: error: expected ';' before 'print_world'
      6 |   printf("Hello ")
        |                   ^
        |                   ;
      7 |   print_world();
        |   ~~~~~~~~~~~      
  gcc -o hello hello.c world.c

Both broken -> Both working
  $ cp versions/hello-working.c hello.c
  $ cp versions/world-working.c world.c
  $ rkr --show --no-wrapper
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

Both working -> Broken world.c
  $ cp versions/world-broken.c world.c
  $ rkr --show --no-wrapper
  cc1 * (glob)
  world.c: In function 'print_world':
  world.c:4:21: error: expected ';' before '}' token
      4 |   printf("world.\n")
        |                     ^
        |                     ;
      5 | }
        | ~                    
  gcc -o hello hello.c world.c
  Rikerfile

Broken world.c -> Broken hello.c
  $ cp versions/hello-broken.c hello.c
  $ cp versions/world-working.c world.c
  $ rkr --show --no-wrapper
  cc1 * (glob)
  hello.c: In function 'main':
  hello.c:6:19: error: expected ';' before 'print_world'
      6 |   printf("Hello ")
        |                   ^
        |                   ;
      7 |   print_world();
        |   ~~~~~~~~~~~      
  cc1 * (glob)
  gcc -o hello hello.c world.c
  as * (glob)

Broken hello.c -> Both broken
  $ cp versions/world-broken.c world.c
  $ rkr --show --no-wrapper
  cc1 * (glob)
  world.c: In function 'print_world':
  world.c:4:21: error: expected ';' before '}' token
      4 |   printf("world.\n")
        |                     ^
        |                     ;
      5 | }
        | ~                    
  gcc -o hello hello.c world.c

Both broken -> Broken hello.c
  $ cp versions/world-working.c world.c
  $ rkr --show --no-wrapper
  cc1 * (glob)
  gcc -o hello hello.c world.c
  as * (glob)

Broken hello.c -> Both working
  $ cp versions/hello-working.c hello.c
  $ rkr --show --no-wrapper
  cc1 * (glob)
  gcc -o hello hello.c world.c
  as * (glob)
  collect2 * (glob)
  ld * (glob)
  Rikerfile

Check the output
  $ ./hello
  Hello world.

Both working -> Both broken
  $ cp versions/hello-broken.c hello.c
  $ cp versions/world-broken.c world.c
  $ rkr --show --no-wrapper
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
  gcc -o hello hello.c world.c
  Rikerfile

Both broken -> Broken world.c
  $ cp versions/hello-working.c hello.c
  $ rkr --show --no-wrapper
  cc1 * (glob)
  gcc -o hello hello.c world.c
  as * (glob)

Broken world.c -> Both working
  $ cp versions/world-working.c world.c
  $ rkr --show --no-wrapper
  cc1 * (glob)
  gcc -o hello hello.c world.c
  as * (glob)
  collect2 * (glob)
  ld * (glob)
  Rikerfile

Check the output
  $ ./hello
  Hello world.

Clean up
  $ rm -rf .rkr
  $ rm hello
  $ cp versions/hello-working.c hello.c
  $ cp versions/world-working.c world.c

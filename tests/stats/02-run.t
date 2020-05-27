Move to test directory
  $ cd $TESTDIR

SKIP!
  $ exit 80

Run the build
  $ ../../dodo --show
  dodo launch
  Dodofile
  ./A
  cat inputA
  ./B
  cat inputB

Verify the output is correct
  $ ../../dodo stats
  Build Statistics: (re)
    Commands: [0-9]+ (re)
    Steps: [0-9]+ (re)
    Artifacts: [0-9]+ (re)
    Artifact Versions: [0-9]+ (re)

Verify the -a output is correct
  $ ../../dodo stats -a | head -n 8
  Build Statistics:
    Commands: [0-9]+ (re)
    Steps: [0-9]+ (re)
    Artifacts: [0-9]+ (re)
    Artifact Versions: [0-9]+ (re)
  
  Artifacts:
    .+ (re)
Move to test directory
  $ cd $TESTDIR

Run the build
  $ rkr --show
  rkr-launch
  Rikerfile
  ./A
  cat inputA
  ./B
  cat inputB

Verify the output is correct
  $ rkr stats
  Build Statistics: (re)
    Commands: [0-9]+ (re)
    Steps: [0-9]+ (re)
    Artifacts: [0-9]+ (re)
    Artifact Versions: [0-9]+ (re)

Verify the -a output is correct
  $ rkr stats -a | head -n 8
  Build Statistics:
    Commands: [0-9]+ (re)
    Steps: [0-9]+ (re)
    Artifacts: [0-9]+ (re)
    Artifact Versions: [0-9]+ (re)
  
  Artifacts:
    .+ (re)
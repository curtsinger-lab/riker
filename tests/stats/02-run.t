Move to test directory
  $ cd $TESTDIR

Run the build
  $ $DODO --show
  dodo-launch
  Dodofile
  ./A
  cat inputA
  ./B
  cat inputB

Verify the output is correct
  $ $DODO stats
  Build Statistics: (re)
    Commands: [0-9]+ (re)
    Steps: [0-9]+ (re)
    Artifacts: [0-9]+ (re)
    Artifact Versions: [0-9]+ (re)

Verify the -a output is correct
  $ $DODO stats -a | head -n 8
  Build Statistics:
    Commands: [0-9]+ (re)
    Steps: [0-9]+ (re)
    Artifacts: [0-9]+ (re)
    Artifact Versions: [0-9]+ (re)
  
  Artifacts:
    .+ (re)
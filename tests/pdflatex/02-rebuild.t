Move to test directory
  $ cd $TESTDIR

Clean up any leftover state
  $ rm -rf .rkr
  $ rm -f main.aux main.log main.pdf
  $ cp main-original.tex main.tex

Run the build
  $ rkr --fingerprint all --show
  rkr-launch
  Rikerfile
  pdflatex --interaction=batchmode main.tex
  pdflatex --interaction=batchmode main.tex

Check for the output pdf
  $ file main.pdf
  main.pdf: PDF document, * (glob)

Run a rebuild (should do nothing)
  $ rkr --fingerprint all --show

Clean up
  $ rm -rf .rkr
  $ rm -f main.aux main.log main.pdf

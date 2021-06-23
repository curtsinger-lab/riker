Move to test directory
  $ cd $TESTDIR

Clean up any leftover state
  $ rm -rf .rkr
  $ rm -f main.aux main.log main.pdf
  $ cp main-original.tex main.tex

Run the build
  $ rkr --show
  rkr-launch
  Rikerfile
  pdflatex main.tex
  pdflatex main.tex

Check for the output pdf
  $ file main.pdf
  main.pdf: PDF document, * (glob)

Clean up
  $ rm -rf .rkr
  $ rm -f main.aux main.log main.pdf

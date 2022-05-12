Move to test directory
  $ cd $TESTDIR

Is pdflatex installed? If not, skip this test
  $ which pdflatex > /dev/null || exit 80

Clean up any leftover state
  $ rm -rf .rkr
  $ rm -f main.aux main.log main.pdf
  $ cp main-original.tex main.tex

Run the build
  $ rkr --show
  rkr-launch
  Rikerfile
  pdflatex --interaction=batchmode main.tex
  pdflatex --interaction=batchmode main.tex

Check for the output pdf
  $ file main.pdf
  main.pdf: PDF document, * (glob)

Clean up
  $ rm -rf .rkr
  $ rm -f main.aux main.log main.pdf

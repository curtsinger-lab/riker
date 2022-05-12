Move to test directory
  $ cd $TESTDIR

Is pdflatex installed? If not, skip this test
  $ which pdflatex > /dev/null || exit 80

Clean up any leftover state
  $ rm -rf .rkr
  $ rm -f main.aux main.log main.pdf
  $ cp main-original.tex main.tex

Run the build
  $ rkr --show --fingerprint all
  rkr-launch
  Rikerfile
  pdflatex --interaction=batchmode main.tex
  pdflatex --interaction=batchmode main.tex

Check for the output pdf
  $ file main.pdf
  main.pdf: PDF document, * (glob)

Copy in a version with edited text (no reference changes)
  $ cp main-edited-ref.tex main.tex

Run a rebuild. Both pdflatexs must run because references changed.
  $ rkr --show --fingerprint all
  pdflatex --interaction=batchmode main.tex
  pdflatex --interaction=batchmode main.tex

Run a rebuild (should do nothing)
  $ rkr --show --fingerprint all

Clean up
  $ rm -rf .rkr
  $ rm -f main.aux main.log main.pdf
  $ cp main-original.tex main.tex

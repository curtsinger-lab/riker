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
  pdflatex --interaction=batchmode main.tex
  pdflatex --interaction=batchmode main.tex

Check for the output pdf
  $ file main.pdf
  main.pdf: PDF document, * (glob)

Copy in a version with edited text (no reference changes)
  $ cp main-edited-ref.tex main.tex

Run a rebuild. Both pdflatexs must run because references changed.
  $ rkr --show
  pdflatex --interaction=batchmode main.tex
  pdflatex --interaction=batchmode main.tex

Clean up
  $ rm -rf .rkr
  $ rm -f main.aux main.log main.pdf
  $ cp main-original.tex main.tex

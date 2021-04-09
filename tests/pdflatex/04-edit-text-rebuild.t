Move to test directory
  $ cd $TESTDIR

Clean up any leftover state
  $ rm -rf .rkr
  $ rm -f main.aux main.log main.pdf
  $ cp main-original.tex main.tex

Run the build
  $ $RKR --show
  rkr-launch
  Rikerfile
  pdflatex main.tex
  pdflatex main.tex

Check for the output pdf
  $ file main.pdf
  main.pdf: PDF document, * (glob)

Copy in a version with edited text (no reference changes)
  $ cp main-edited-text.tex main.tex

Run a rebuild. If we could determine that the second pdflatex invocation is equivalent to the first we could skip it, but that's future work.
  $ $RKR --show
  pdflatex main.tex
  pdflatex main.tex

Run a rebuild (should do nothing)
  $ $RKR --show

Clean up
  $ rm -rf .rkr
  $ rm -f main.aux main.log main.pdf
  $ cp main-original.tex main.tex

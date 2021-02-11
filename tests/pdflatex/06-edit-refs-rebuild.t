Move to test directory
  $ cd $TESTDIR

Clean up any leftover state
  $ rm -rf .rkr
  $ rm -f main.aux main.log main.pdf
  $ cp main-original.tex main.tex

Run the build
  $ $RKR --show --fingerprint all
  rkr-launch
  Rikerfile
  pdflatex main.tex
  pdflatex main.tex

Check for the output pdf
  $ file main.pdf
  main.pdf: PDF document, * (glob)

Copy in a version with edited text (no reference changes)
  $ cp main-edited-ref.tex main.tex

Run a rebuild. Both pdflatexs must run because references changed.
  $ $RKR --show --fingerprint all
  pdflatex main.tex
  pdflatex main.tex

Run a rebuild (should do nothing)
  $ $RKR --show --fingerprint all

Clean up
  $ rm -rf .rkr
  $ rm -f main.aux main.log main.pdf
  $ cp main-original.tex main.tex

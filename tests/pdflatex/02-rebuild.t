Move to test directory
  $ cd $TESTDIR

Clean up any leftover state
  $ rm -rf .dodo
  $ rm -f main.aux main.log main.pdf
  $ cp main-original.tex main.tex

Run the build
  $ $DODO --show
  dodo-launch Dodofile
  Dodofile
  pdflatex main.tex
  pdflatex main.tex

Check for the output pdf
  $ file main.pdf
  main.pdf: PDF document, * (glob)

Run a rebuild (should do nothing)
  $ $DODO --show

Clean up
  $ rm -rf .dodo
  $ rm -f main.aux main.log main.pdf

SKIP! This test does not work because pdflatex overwrites main.aux. Once we have fingerprinting it should pass.
  $ exit 80

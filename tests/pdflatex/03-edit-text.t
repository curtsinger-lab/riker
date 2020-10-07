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

Copy in a version with edited text (no reference changes)
  $ cp main-edited-text.tex main.tex

Run a rebuild (should just run one pdflatex)
  $ $DODO --show
  pdflatex main.tex

Clean up
  $ rm -rf .dodo
  $ rm -f main.aux main.log main.pdf
  $ cp main-original.tex main.tex

SKIP! This test will not work until we have the ability to skip recognized commands.
  $ exit 80

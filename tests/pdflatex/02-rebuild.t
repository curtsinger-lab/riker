Move to test directory
  $ cd $TESTDIR

Clean up any leftover state
  $ rm -rf .dodo
  $ rm -f main.aux main.log main.pdf
  $ cp main-original.tex main.tex

Run the build
  $ $DODO --fingerprint all --show
  dodo-launch
  Rikerfile
  pdflatex main.tex
  pdflatex main.tex

Check for the output pdf
  $ file main.pdf
  main.pdf: PDF document, * (glob)

Run a rebuild (should do nothing)
  $ $DODO --fingerprint all --show

Clean up
  $ rm -rf .dodo
  $ rm -f main.aux main.log main.pdf

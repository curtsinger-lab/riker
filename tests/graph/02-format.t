Move to test directory
  $ cd $TESTDIR

Clean up any previous build
  $ rm -rf .dodo output out.png out.pdf out.jpg

Run a build
  $ $DODO

Generate graph output in png format
  $ $DODO graph --type png

Check for the rendered build graph
  $ file out.png
  out.png: PNG image.* (re)

Generate graph output in pdf form
  $ $DODO graph --type pdf

Check for the rendered build graph
  $ file out.pdf
  out.pdf: PDF document.* (re)

Generate graph output in jpg form
  $ $DODO graph --type jpg

Check for the rendered build graph
  $ file out.jpg
  out.jpg: JPEG image.* (re)

Clean up
  $ rm -rf .dodo output out.png out.pdf out.jpg

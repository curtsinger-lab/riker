Move to test directory
  $ cd $TESTDIR

Clean up any previous build
  $ rm -rf .rkr output out.png out.pdf out.jpg

Run a build
  $ $RKR

Generate graph output in png format
  $ $RKR graph --type png

Check for the rendered build graph
  $ file out.png
  out.png: PNG image.* (re)

Generate graph output in pdf form
  $ $RKR graph --type pdf

Check for the rendered build graph
  $ file out.pdf
  out.pdf: PDF document.* (re)

Generate graph output in jpg form
  $ $RKR graph --type jpg

Check for the rendered build graph
  $ file out.jpg
  out.jpg: JPEG image.* (re)

Clean up
  $ rm -rf .rkr output out.png out.pdf out.jpg

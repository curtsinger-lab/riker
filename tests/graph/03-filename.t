Move to test directory
  $ cd $TESTDIR

Clean up any previous build
  $ rm -rf .rkr output A1.png A2.png B1.pdf B2.pdf C1.jpg C2.jpg

Run a build
  $ $RKR

Generate graph output in png format, once with an extension and once without
  $ $RKR graph -o A1
  $ $RKR graph -o A2.png

Check for the rendered build graph
  $ file A1.png A2.png
  A1.png: PNG image.* (re)
  A2.png: PNG image.* (re)

Generate graph output in pdf form, once with an extension and once without
  $ $RKR graph --type pdf -o B1
  $ $RKR graph --type pdf -o B2.pdf

Check for the rendered build graph
  $ file B1.pdf B2.pdf
  B1.pdf: PDF document.* (re)
  B2.pdf: PDF document.* (re)

Generate graph output in jpg form, once with an extension and once without
  $ $RKR graph --type jpg -o C1
  $ $RKR graph --type jpg -o C2.jpg

Check for the rendered build graph
  $ file C1.jpg C2.jpg
  C1.jpg: JPEG image.* (re)
  C2.jpg: JPEG image.* (re)

Clean up
  $ rm -rf .rkr output A1.png A2.png B1.pdf B2.pdf C1.jpg C2.jpg

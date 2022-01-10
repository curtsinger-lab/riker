module Main where

import Development.Rattle

main :: IO ()
main = rattleRun rattleOptions $ do
  cmd "pdflatex  main.tex"
  cmd "bibtex  main.aux"
  cmd "pdflatex  main.tex"
  cmd "pdflatex  main.tex"
  cmd "pdflatex  extended-abstract.tex"
  cmd "bibtex  extended-abstract.aux"
  cmd "pdflatex  extended-abstract.tex"
  cmd "pdflatex  extended-abstract.tex"


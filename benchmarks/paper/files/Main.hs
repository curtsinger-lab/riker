module Main where

import Development.Rattle

main :: IO ()
main = rattleRun rattleOptions $ do
  cmd Shell "pdflatex  main.tex"
  cmd Shell "bibtex  main.aux"
  cmd Shell "pdflatex  main.tex"
  cmd Shell "pdflatex  main.tex"
  cmd Shell "pdflatex  extended-abstract.tex"
  cmd Shell "bibtex  extended-abstract.aux"
  cmd Shell "pdflatex  extended-abstract.tex"
  cmd Shell "pdflatex  extended-abstract.tex"


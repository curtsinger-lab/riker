module Main where

import Development.Rattle

main :: IO ()
main = rattleRun rattleOptions $ do
  cmd "make  clean  all  --quiet"


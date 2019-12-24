module Main where

import qualified Day.One   as D01
import qualified Day.Two   as D02
import qualified Day.Three as D03
import qualified Day.Four  as D04

main :: IO ()
main = do
  putStrLn "Day 1:"
  print =<< D01.part1
  print =<< D01.part2
  putStrLn "Day 2:"
  print =<< D02.part1
  print =<< D02.part2
  putStrLn "Day 3:"
  print =<< D03.part1
  print =<< D03.part2
  putStrLn "Day 4:"
  print =<< D04.part1
  print =<< D04.part2

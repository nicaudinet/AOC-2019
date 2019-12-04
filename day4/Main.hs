module Main where

import Data.List (sort, group)

input :: [Int]
input = [134564 .. 585159]

hasDoubleDigit :: Int -> Bool
hasDoubleDigit = not . null . filter (> 1) . map length . group . show

alwaysIncreasing :: Int -> Bool
alwaysIncreasing n = let n' = show n in sort n' == n'

hasOnlyDoubleDigit :: Int -> Bool
hasOnlyDoubleDigit = not . null . filter (== 2) . map length . group . show

main :: IO ()
main = do
  print (length [ x | x <- input, alwaysIncreasing x, hasDoubleDigit x ])
  print (length [ x | x <- input, alwaysIncreasing x, hasOnlyDoubleDigit x ])

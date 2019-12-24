module Day.Four where

import Data.List (sort, group)

input :: [Int]
input = [134564 .. 585159]

hasDoubleDigit :: Int -> Bool
hasDoubleDigit = not . null . filter (> 1) . map length . group . show

alwaysIncreasing :: Int -> Bool
alwaysIncreasing n = let n' = show n in sort n' == n'

hasOnlyDoubleDigit :: Int -> Bool
hasOnlyDoubleDigit = not . null . filter (== 2) . map length . group . show

part1 :: IO Int
part1 =
  pure (length [ x | x <- input, alwaysIncreasing x, hasDoubleDigit x ])

part2 :: IO Int
part2 =
  pure (length [ x | x <- input, alwaysIncreasing x, hasOnlyDoubleDigit x ])

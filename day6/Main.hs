module Main where

import Data.List (break)
import Data.Bifunctor (second)

parseOrbits :: String -> [(String, String)]
parseOrbits = map (second tail . break (== ')')) . lines

next :: [(String, String)] -> [String] -> [String]
next orbits curr = map snd (filter (flip elem curr . fst) orbits)

fix :: Eq a => (a -> a) -> a -> [a]
fix f a =
  let result = f a
  in a : if result == a then [] else fix f result

orbitTree :: [(String, String)] -> [[String]]
orbitTree orbits = fix (next orbits) ["COM"]

countOrbits :: [[String]] -> Int
countOrbits = sum . map (\(i, os) -> i * length os) . zip [0..]

main :: IO ()
main = do
  orbits <- parseOrbits <$> readFile "input"
  print (countOrbits $ orbitTree orbits)

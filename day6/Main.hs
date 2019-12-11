module Main where

import Data.List
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

dupe :: a -> (a,a)
dupe x = (x,x)

routeToCOM :: [(String, String)] -> String -> [String]
routeToCOM orbits = unfoldr go
  where
    go :: String -> Maybe (String, String)
    go object = dupe . fst <$> find ((== object) . snd) orbits

route :: [(String, String)] -> String -> String -> [String]
route orbits a b =
  let routeA = routeToCOM orbits a
      routeB = routeToCOM orbits b
  in routeA `union` routeB \\ routeA `intersect` routeB

main :: IO ()
main = do
  orbits <- parseOrbits <$> readFile "input"
  print (countOrbits $ orbitTree orbits)
  print (length $ route orbits "YOU" "SAN")

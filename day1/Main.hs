module Main where

import Data.List (unfoldr)

moduleFuel :: Int -> Int
moduleFuel x = (x `div` 3) - 2

fuelFuel :: Int -> Int
fuelFuel = sum . unfoldr fuel
  where
    fuel :: Int -> Maybe (Int, Int)
    fuel x =
      let y = moduleFuel x
      in if y <= 0 then Nothing else Just (y, y)

totalFuel :: Int -> Int
totalFuel x = moduleFuel x + fuelFuel (moduleFuel x)

main :: IO ()
main = do
  modules <- map read . lines <$> readFile "input"
  print (sum $ map moduleFuel modules)
  print (sum $ map totalFuel modules)

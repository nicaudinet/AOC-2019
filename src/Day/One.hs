module Day.One where

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

part1 :: IO Int
part1 = do
  modules <- map read . lines <$> readFile "inputs/day1"
  pure (sum $ map moduleFuel modules)

part2 :: IO Int
part2 = do
  modules <- map read . lines <$> readFile "inputs/day1"
  pure (sum $ map totalFuel modules)

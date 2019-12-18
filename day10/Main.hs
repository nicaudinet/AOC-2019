{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DeriveFunctor #-}

module Main where

import Data.Bifunctor (bimap)
import Data.List
import Data.Ratio
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import Debug.Trace

data Asteroid = Empty | Asteroid
  deriving (Eq, Show)

parseAsteroid :: Char -> Asteroid
parseAsteroid '.' = Empty
parseAsteroid '#' = Asteroid

showAsteroid :: Asteroid -> Char
showAsteroid Empty = '.'
showAsteroid Asteroid = '#'

type Coord = (Int, Int)

newtype Grid a = Grid { unGrid :: (Vector (Vector a)) }
  deriving (Show, Functor)

parseGrid :: String -> Grid Asteroid
parseGrid = Grid . V.fromList . map (V.fromList . map parseAsteroid) . lines

cell :: Grid a -> Coord -> a
cell (Grid grid) (x,y) = (grid ! y) ! x

bounds :: Grid a -> (Int, Int)
bounds (Grid grid) = (length (V.head grid), length grid)

translate :: Coord -> Coord -> Coord
translate (x,y) = bimap (subtract x) (subtract y)

coords :: Grid a -> Grid Coord
coords grid = Grid (fmap makeRow [0..(y-1)])
  where 
    (x,y) = bounds grid
    makeRow y = V.fromList [ (a,y) | a <- [0..(x-1)] ]

flatten :: Grid a -> [a]
flatten (Grid grid) = concat (map V.toList (V.toList grid))

-- Euclid's algorithm to find the Lowest Common demonimator of two integers
euclid :: Int -> Int -> Maybe Int
euclid x y
  | a <= 0 = Nothing
  | b <= 0 = Nothing
  | a == b = Just a
  | True   = euclid b (a - b)
  where
    a = max x y
    b = min x y

range :: Int -> Int -> [Int]
range x y = [ (min x y + 1) .. (max x y - 1) ]

-- All the points between the coordinate and the origin in a straight line for a
-- coordinate with only natural numbers
between :: Coord -> [Coord]
between (0,0) = []
between (x,0) = zip (range 0 x) (repeat 0)
between (0,y) = zip (repeat 0) (range 0 y)
between (x,y) = zip [x', 2*x' .. (x - x')] [y', 2*y' .. (y - y')]
  where
    Just lcd = euclid (abs x) (abs y)
    x' = x `div` lcd
    y' = y `div` lcd

betweens :: Coord -> Coord -> [Coord]
betweens (x,y) = fmap (translate (-x,-y)) . between . translate (x,y)

visible :: Grid Asteroid -> Coord -> Coord -> Bool
visible asteroids center coord
  | center == coord = False
  | cell asteroids coord == Empty = False
  | otherwise = all ((== Empty) . cell asteroids) (betweens center coord)

count :: (a -> Bool) -> Grid a -> Int
count f = length . filter id . flatten . fmap f

countVisible :: Grid Asteroid -> Coord -> Int
countVisible asteroids center
  | cell asteroids center == Empty = 0
  | otherwise = count (visible asteroids center) (coords asteroids)

detect :: Grid Asteroid -> Grid Int
detect asteroids = fmap (countVisible asteroids) (coords asteroids)

type Angle = Float

angle :: Coord -> Angle
angle (0, _)  = pi / 2
angle (run,rise) = atan ((fromIntegral rise) / (fromIntegral run))

gatherVisible :: Grid Asteroid -> Coord -> [Coord]
gatherVisible asteroids center
  | cell asteroids center == Empty = []
  | otherwise = filter (visible asteroids center) (flatten (coords asteroids))

delta :: Angle -> Coord -> (Angle, Coord)
delta angle (x,y) = undefined

main :: IO ()
main = do
  asteroids <- parseGrid <$> readFile "input"
  print (maximum (flatten (detect asteroids)))

prettyPrint :: Show a => Grid a -> IO ()
prettyPrint (Grid grid) = mapM_ print grid

test0 :: IO ()
test0 = do
  grid <- readFile "testInput0"
  putStrLn grid
  let asteroids = parseGrid grid
  let fieldOfVision = fmap (visible asteroids (3,4)) (coords asteroids)
  prettyPrint fieldOfVision
  putStrLn "\n"
  let fieldOfVision = fmap (visible asteroids (4,4)) (coords asteroids)
  print (translate (4,4) (4,0))
  print (between $ translate (4,4) (4,0))
  print (betweens (4,4) (4,0))
  print (visible asteroids (4,4) (4,0))
  prettyPrint fieldOfVision
  putStrLn "\n"
  prettyPrint (detect asteroids)
  print (maximum (flatten (detect asteroids)))

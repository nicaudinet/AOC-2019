module Main where

import Data.Bifunctor (bimap)
import Data.List
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import Debug.Trace

data Area = Empty | Asteroid
  deriving Eq

parseArea :: Char -> Area
parseArea '.' = Empty
parseArea '#' = Asteroid

showArea :: Area -> Char
showArea Empty = '.'
showArea Asteroid = '#'

type Coord = (Int, Int)
type Grid = Vector (Vector Area)
type Bounds = ((Int, Int), (Int, Int))

parseGrid :: String -> Grid
parseGrid = V.fromList . map V.fromList . map (map parseArea) . lines

bounds :: Grid -> Bounds
bounds grid = ((0, 0), (maxX, maxY))
  where
    maxX = V.length (V.head grid) - 1
    maxY = V.length grid - 1

cell :: Grid -> Coord -> Area
cell grid (x,y) =
  if x < minX || x > maxX
  then Empty
  else
    if y < minY || y > maxY
    then Empty
    else (grid ! y) ! x
  where ((minX, minY), (maxX, maxY)) = bounds grid

surrounding :: Coord -> Int -> [Coord]
surrounding (x,y) n = outer \\ inner
  where
    outerNs = [(-n)..n]
    innerNs = [(1-n)..(n-1)]
    outer = [ (x+a, y+b) | a <- outerNs, b <- outerNs ]
    inner = [ (x+a, y+b) | a <- innerNs, b <- innerNs ]

-- isMultiple :: Int -> Int -> Bool
-- isMultiple 0 0 = True
-- isMultiple _ 0 = False
-- isMultiple a b = a `mod` b == 0

gradient :: Coord -> Coord -> Double
gradient (xx, xy) (ax, ay)
  | xx - ax == 0 = 1000000000
  | otherwise    = (fromIntegral $ ay - xy) / (fromIntegral $ ax - xx)

bigger :: Coord -> Coord -> Coord -> Bool
bigger (xx, xy) (ax, ay) (bx, by) =
  let aTox = (abs $ xx - ax) + (abs $ xy - ay)
      bTox = (abs $ xx - bx) + (abs $ xy - by)
  in aTox > bTox

-- Center, is A behind B
isBehind :: Coord -> Coord -> Coord -> Bool
isBehind x a b = (gradient x a == gradient x b) && bigger x a b

-- From a center, is A behind any B
isHidden :: Coord -> Coord -> [Coord] -> Bool
isHidden center a = any (isBehind center a)

maxDim :: Grid -> Int
maxDim = maximum . (\(a,b) -> [a,b]) . snd . bounds

surroundings :: Grid -> Coord -> [[Coord]]
surroundings grid center = reverse $ map (surrounding center) [1 .. maxDim grid]

go :: Grid -> Coord -> [Coord] -> [Coord] -> [Coord]
go grid center toCheck visible =
  let asteroids = filter ((== Asteroid) . cell grid) toCheck
      alsoVisible = filter (not . flip (isHidden center) visible) asteroids
  in alsoVisible <> visible
  
visibleAsteroids :: Grid -> Coord -> [Coord]
visibleAsteroids grid center = 
  foldr (go grid center) [] (surroundings grid center)

type Location = (Int, Coord)

location :: Grid -> Coord -> Location
location grid coord =
  let num = length $ visibleAsteroids grid coord
  in (num, coord)

locationVisibility :: Grid -> [Location]
locationVisibility grid =
  [ location grid (a,b) | a <- [minX..maxX], b <- [minY..maxY], cell grid (a,b) == Asteroid ]
  where ((minX, minY), (maxX, maxY)) = bounds grid

cmp :: Location -> Location -> Ordering
cmp (l1,_) (l2,_) = compare l1 l2

bestLocation :: [Location] -> Location
bestLocation = maximumBy cmp

main :: IO ()
main = do
  grid <- parseGrid <$> readFile "input"
  print (bestLocation . locationVisibility $ grid)

test :: IO ()
test = test0 >> test1

test0 :: IO ()
test0 = do
  grid <- parseGrid <$> readFile "testInput0"
  let visibility = locationVisibility grid
  mapM_ print (map fst visibility)
  print (bestLocation visibility)

test1 :: IO ()
test1 = do
  grid <- parseGrid <$> readFile "testInput1"
  print (bestLocation . locationVisibility $ grid)

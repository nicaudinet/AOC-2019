{-# LANGUAGE OverloadedLists #-}

module Main where

import Data.List (foldl')
import Data.List.Split (splitOn)
import qualified Data.Set as S

data Dir = U | D | R | L
  deriving (Show, Read)

data Instr = Instr Dir Int
  deriving Show

type Wire = [Instr]
type Loc  = (Int, Int)
type Locs = S.Set Loc

parse :: String -> Wire
parse = map parseInstr . splitOn ","

parseInstr :: String -> Instr
parseInstr (dir : num) = Instr (read [dir]) (read num)

between :: Int -> Int -> [Int]
between a b
  | a < b  = [a + 1 .. b]
  | a == b = [a]
  | a > b  = reverse [b .. a - 1]

locations :: Loc -> Loc -> [Loc]
locations (x1, y1) (x2, y2) =
  [ (a, b) | a <- between x1 x2, b <- between y1 y2 ]

move :: Loc -> Instr -> Loc
move (x, y) (Instr dir amount) =
  case dir of
    U -> (x, y + amount)
    D -> (x, y - amount)
    R -> (x + amount, y)
    L -> (x - amount, y)

wireLocs :: Wire -> [Loc]
wireLocs = snd . foldl' go ((0, 0), [])
  where
    go :: (Loc, [Loc]) -> Instr -> (Loc, [Loc])
    go (current, past) instr =
      let next = move current instr
      in (next, past <> locations current next)

crossings :: Wire -> Wire -> Locs
crossings w1 w2 =
  S.intersection (S.fromList $ wireLocs w1) (S.fromList $ wireLocs w2)

minDistance :: Locs -> Int
minDistance = S.findMin . S.map (\(x,y) -> abs x + abs y)

steps :: [Loc] -> Loc -> Int
steps wire destination = go wire 0
  where
    go :: [Loc] -> Int -> Int
    go (l:ls) i = if l == destination then i else go ls (i + 1)
    go []     _ = error "Location was not in the list"

minSteps :: Wire -> Wire -> Int
minSteps w1 w2 =
  let xs = crossings w1 w2
      ls1 = wireLocs w1
      ls2 = wireLocs w2
      allSteps = S.map (\loc -> steps ls1 loc + steps ls2 loc) xs
  in S.findMin allSteps

test1 :: IO ()
test1 = do
  let w1 = parse "R75,D30,R83,U83,L12,D49,R71,U7,L72"
  let w2 = parse "U62,R66,U55,R34,D71,R55,D58,R83"
  print (minSteps w1 w2) -- should be 610

test2 :: IO ()
test2 = do
  let w1 = parse "R8,U5,L5,D3"
  let w2 = parse "U7,R6,D4,L4"
  print (minSteps w1 w2) -- should be 30

main :: IO ()
main = do
  [a, b] <- lines <$> readFile "input"
  let w1 = parse a
      w2 = parse b
      xs = crossings w1 w2
  print (minDistance xs)
  print (minSteps w1 w2)

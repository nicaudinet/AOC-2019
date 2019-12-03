{-# LANGUAGE OverloadedLists #-}

module Main where

import Data.List (foldl', nub)
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

locations :: Loc -> Loc -> Locs
locations (x1, y1) (x2, y2) = S.fromList
  [ (a, b) | a <- between x1 x2, b <- between y1 y2 ]

move :: Loc -> Instr -> Loc
move (x, y) (Instr dir amount) =
  case dir of
    U -> (x, y + amount)
    D -> (x, y - amount)
    R -> (x + amount, y)
    L -> (x - amount, y)

between :: Int -> Int -> [Int]
between a b
  | a < b  = [a .. b]
  | a == b = [a]
  | a > b  = reverse [b .. a]

wireLocs :: Wire -> Locs
wireLocs = snd . foldl' go ((0, 0), [(0, 0)])
  where
    go :: (Loc, Locs) -> Instr -> (Loc, Locs)
    go (current, past) instr =
      let next = move current instr
      in (next, past <> locations current next)

crossings :: Wire -> Wire -> Locs
crossings w1 w2 =
  S.intersection (wireLocs w1) (wireLocs w2)

minDistance :: Locs -> Int
minDistance = S.findMin . S.deleteMin . S.map (\(x,y) -> abs x + abs y)

main :: IO ()
main = do
  [wire1, wire2] <- lines <$> readFile "input"
  let xs = crossings (parse wire1) (parse wire2)
  print (minDistance xs)

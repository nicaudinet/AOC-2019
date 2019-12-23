module Main where

import IntCode

import Data.List
import qualified Data.Map as M

data Tile = Empty | Wall | Block | Paddle | Ball | Score Int
  deriving Eq
type Pos = (Int,Int)
type Display =M.Map Pos Tile 
type Screen = (Int, Display)
type ArcadeState = (ComputerState, Screen)

data JoyStick = L | R

intToTile :: Int -> Tile
intToTile 0 = Empty
intToTile 1 = Wall
intToTile 2 = Block
intToTile 3 = Paddle
intToTile 4 = Ball
intToTile i = error ("Tile ID " <> show i <> " is not valid.")

tileToChar :: Tile -> Char
tileToChar Empty = ' '
tileToChar Wall = '%'
tileToChar Block = '#'
tileToChar Paddle = '_'
tileToChar Ball = '*'

buildScreen :: [Int] -> Screen
buildScreen output = go output (0, mempty)
  where
    go :: [Int] -> Screen -> Screen
    go (-1 : 0 : score : rest) (_, display) = go rest (score, display)
    go (x : y : tileId : rest) (score, display) =
      go rest (score, M.insert (x,y) (intToTile tileId) display)
    go _ screen = screen

drawScreen :: Screen -> String
drawScreen (score, display) =
  let displayString = showGrid (toGridDisplay display)
  in "Score: " <> show score <> "\n" <> displayString

showGrid :: [[(Pos, Tile)]] -> String
showGrid = unlines . map (map (tileToChar . snd))

chunk :: Int -> [a] -> [[a]]
chunk n [] = []
chunk n lst = take n lst : chunk n (drop n lst)

toGridDisplay :: Display -> [[(Pos, Tile)]]
toGridDisplay
  = map (sortBy compareX)
  . chunk 44
  . sortBy compareY
  . M.toList
  where
    compareX :: ((Int,Int),Tile) -> ((Int,Int),Tile) -> Ordering
    compareX ((x1,_),_) ((x2,_),_) = compare x1 x2

    compareY :: ((Int,Int),Tile) -> ((Int,Int),Tile) -> Ordering
    compareY ((_,y1),_) ((_,y2),_) = compare y1 y2

    sameY :: ((Int,Int),Tile) -> ((Int,Int),Tile) -> Bool
    sameY ((_,y1),_) ((_,y2),_) = y1 == y2

main :: IO ()
main = do
  program <- parseContents <$> readFile "input"
  let finishedState = run (initState program)
      output = reverse . map fromInteger . stateOutput $ finishedState
      screen = buildScreen output
  print (length . filter (== Block) . M.elems . snd $ screen)
  putStrLn (drawScreen screen)
  print (map (map fst) . toGridDisplay $ snd screen)

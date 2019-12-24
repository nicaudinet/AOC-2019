module Main where

import IntCode

import Control.Monad
import System.Console.ANSI (clearScreen)
import Data.List
import qualified Data.Map as M

data Tile = Empty | Wall | Block | Paddle | Ball | Score Int
  deriving Eq
type Pos = (Int,Int)
type Display =M.Map Pos Tile 
type Screen = (Int, Display)
type ArcadeState = (ComputerState, Screen)

data JoyStick = L | R | N

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

charToJoyStick :: Char -> JoyStick
charToJoyStick 'j' = L
charToJoyStick 'l' = R
charToJoyStick _   = N

joyStickToInteger :: JoyStick -> Integer
joyStickToInteger L = -1
joyStickToInteger N = 0
joyStickToInteger R = 1

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

step :: ComputerState -> IO ComputerState
step state = do
  joystick <- charToJoyStick <$> getChar
  let newState = run (state { stateInput = [joyStickToInteger joystick], status = Running })
      output = reverse . map fromInteger . stateOutput $ newState
      screen = buildScreen output
  clearScreen
  putStrLn (drawScreen screen)
  pure newState

loop :: (a -> IO a) -> a -> IO a
loop f a = foldM (flip ($)) a (repeat f)

main :: IO ()
main = do
  program <- parseContents <$> readFile "input"
  next0 <- step (initState program)
  next1 <- step next0
  next2 <- step next1
  next3 <- step next2
  next4 <- step next3
  pure ()

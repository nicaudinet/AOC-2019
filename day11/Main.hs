module Main where

import IntCode
import qualified Data.Map as M
import qualified Data.Vector as V (replicate)
import Data.List (unfoldr)
import Data.Maybe (fromMaybe)
import Debug.Trace

type Hull = M.Map Pos Color
data Direction = U | D | L | R deriving Show
type Pos = (Int,Int)
type RobotState = (ComputerState, Direction, Pos, Hull)
data Color = Black | White deriving Eq

turnRight :: Direction -> Direction
turnRight U = R
turnRight R = D
turnRight D = L
turnRight L = U

turnLeft :: Direction -> Direction
turnLeft U = L
turnLeft L = D
turnLeft D = R
turnLeft R = U

turn :: Integer -> Direction -> Direction
turn 0 = turnLeft
turn 1 = turnRight
turn _ = error "A wrong turn has occurred"

forward :: Pos -> Direction -> Pos
forward (x,y) U = (x, succ y)
forward (x,y) D = (x, pred y)
forward (x,y) R = (succ x, y)
forward (x,y) L = (pred x, y)

colorToNum :: Num a => Color -> a
colorToNum Black = 0
colorToNum White = 1

numToColor :: (Eq a, Num a) => a -> Color
numToColor 0 = Black
numToColor 1 = White

detect :: Pos -> Hull -> Color
detect pos hull = fromMaybe Black (hull M.!? pos)

initRobot :: Array -> RobotState
initRobot program = (initState program, U, (0,0), mempty)

runRobot :: RobotState -> Hull
runRobot = last . unfoldr step

step :: RobotState -> Maybe (Hull, RobotState)
step (robot, dir, pos, hull) =
  --trace "----------" $
  --trace ("Initial Direction: " <> show dir) $
  --trace ("Intiial Position: " <> show pos) $
  --trace ("Initial Hull: " <> show hull) $
  let panel = detect pos hull
      robot1 = run (robot { stateInput = [colorToNum panel] })
  in
    if status robot1 == Halted
    then Nothing
    else
      let nextHull = M.insert pos (numToColor $ getOutput robot1) hull
          robot2 = run robot1
      in
        if status robot2 == Halted
        then Nothing
        else
          let nextDir = turn (getOutput robot2) dir
              nextPos = forward pos nextDir
              nextState = (robot2, nextDir, nextPos, nextHull)
          in
            --trace "--" $
            --trace ("Panel Input Value: " <> show panel) $
            --trace ("Paint Output Value: " <> show (getOutput paintState)) $
            --trace ("Direction Output Value: " <> show (getOutput dirState)) $
            --trace "--" $
            --trace ("Final Direction: " <> show nextDir) $
            --trace ("Final Position: " <> show nextPos) $
            --trace ("Final Hull: " <> show nextHull) $
            Just (nextHull, nextState)

getOutput :: ComputerState -> Integer
getOutput = head . stateOutput

main :: IO ()
main = do
  program <- parseContents <$> readFile "input"
  let finishedHull = runRobot (initRobot program)
  print (M.size finishedHull)
  --print (length finishedHull)

test :: IO ()
test = do
  program <- parseContents <$> readFile "inputDay9"
  let initState = ComputerState (program <> V.replicate 1000 0) 0 [1] [] 0 Running
  print (head . stateOutput $ run initState)
  print (status $ run initState)

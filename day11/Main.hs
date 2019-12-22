module Main where

import IntCode
import qualified Data.Map as M
import qualified Data.Vector as V (replicate)
import Data.List (unfoldr)
import Data.Maybe (fromMaybe)
import Debug.Trace

type Hull = M.Map Pos Integer
data Direction = U | D | L | R deriving Show
type Pos = (Int,Int)
type RobotState = (ComputerState, Direction, Pos, Hull)

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

forward :: Pos -> Direction -> Pos
forward (x,y) U = (x, succ y)
forward (x,y) D = (x, pred y)
forward (x,y) R = (succ x, y)
forward (x,y) L = (pred x, y)

detect :: Pos -> Hull -> Integer
detect pos hull = fromMaybe 0 (hull M.!? pos)

initRobot :: Array -> RobotState
initRobot program = (initState program, U, (0,0), mempty)

runRobot :: RobotState -> [Hull]
runRobot = take 10 . unfoldr step

step :: RobotState -> Maybe (Hull, RobotState)
step (robot, dir, pos, hull) =
  trace "----------" $
  trace ("Initial Direction: " <> show dir) $
  trace ("Intiial Position: " <> show pos) $
  trace ("Initial Hull: " <> show hull) $
  let panel = detect pos hull
      paintState = run (robot { stateInput = [panel] })
  in
    if status paintState == Halted
    then Nothing
    else
      let nextHull = M.insert pos (getOutput paintState) hull
          dirState = run paintState
      in
        if status dirState == Halted
        then Nothing
        else
          let nextDir = if getOutput dirState == 0 then turnLeft dir else turnRight dir
              nextPos = forward pos nextDir
              nextState = (dirState, nextDir, nextPos, nextHull)
          in
            trace "--" $
            trace ("Panel Input Value: " <> show panel) $
            trace ("Paint Output Value: " <> show (getOutput paintState)) $
            trace ("Direction Output Value: " <> show (getOutput dirState)) $
            trace "--" $
            trace ("Final Direction: " <> show nextDir) $
            trace ("Final Position: " <> show nextPos) $
            trace ("Final Hull: " <> show nextHull) $
            Just (nextHull, nextState)

getOutput :: ComputerState -> Integer
getOutput = head . stateOutput

main :: IO ()
main = do
  program <- parseContents <$> readFile "input"
  let finishedHull = runRobot (initRobot program)
  --print (length $ M.keys finishedHull)
  print (length finishedHull)

test :: IO ()
test = do
  program <- parseContents <$> readFile "inputDay9"
  let initState = ComputerState (program <> V.replicate 1000 0) 0 [1] [] 0 Running
  print (head . stateOutput $ run initState)
  print (status $ run initState)

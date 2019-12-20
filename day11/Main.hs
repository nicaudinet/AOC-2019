module Main where

import IntCode
import qualified Data.Map as M

data Panel = Black | White deriving Eq
type Hull = M.Map Pos Panel
data Direction = U | D | L | R
type Pos = (Int,Int)
type RobotState = (ComputerState, Direction, Pos)

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
forward (x,y) R = (succ x, y)
forward (x,y) D = (x, pred y)
forward (x,y) L = (pred x, y)

panel :: Pos -> Hull -> Panel
panel pos hull = maybe Black id (hull M.!? pos)

initRobot :: Array -> RobotState
initRobot program =
  let cpuState = ComputerState program 0 [0] [] 0 Running
  in (cpuState, U, (0,0))

runRobot :: RobotState -> Hull -> Hull
runRobot robot initHull = undefined

step :: RobotState -> Hull -> (RobotState, Hull)
step (robot, dir, pos) hull =
  let input = if panel pos hull == Black then 0 else 1
      res1 = run (robot { stateInput = input : (stateInput robot) })
      paint = head (stateOutput res1)
      res2 = run (res1)
      nextDir = if head (stateOutput res2) == 0 then turnLeft dir else turnRight dir
      nextPos = forward pos nextDir
  in ((res2, nextDir, nextPos), M.insert pos (if paint == 0 then Black else White) hull)

getProgram :: IO Array
getProgram = parseContents <$> readFile "input"

main :: IO ()
main = do
  program <- getProgram
  let initState = ComputerState program 0 [0] [] 0 Running
      result = run initState
  print (status result)
  print (stateInput result)
  print (stateOutput result)

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Prelude hiding (replicate)
import Control.Monad ((<=<))
import Control.Monad.State
import Data.Function ((&))
import Data.List (find, splitAt, foldl', permutations)
import Data.Vector (Vector, fromList, (!), (//), replicate)

data Mode
  = Position
  | Immediate
  | Relative
  deriving Show

data Value = Value Integer Mode
  deriving Show

data Instr
  = Add Value Value Value
  | Mul Value Value Value
  | Input Value
  | Output Value
  | JumpIfTrue Value Value
  | JumpIfFalse Value Value
  | LessThan Value Value Value
  | Equals Value Value Value
  | RB Value
  | Halt

data Status = Running | Halted
  deriving Eq

type Modes = (Mode, Mode, Mode)
type Array = Vector Integer
type PC = Int
type RelativeBase = Int
type Memory = (Array, PC)
type IOBuffer = [Integer]

data ComputerState = ComputerState
  { stateArray  :: Array
  , statePC     :: PC
  , stateInput  :: IOBuffer
  , stateOutput :: IOBuffer
  , stateRB     :: RelativeBase
  , status      :: Status
  }

type Computer = State ComputerState
type Computation = Computer ()

stateMemory :: ComputerState -> Memory
stateMemory state =
  (stateArray state, statePC state)

memory :: Computer Memory
memory = get >>= pure . stateMemory

fetch :: Value -> Computer Integer
fetch (Value addr Relative) = do
  mem <- stateArray <$> get
  rb  <- stateRB <$> get
  pure $ mem ! (fromInteger addr + rb)
fetch (Value addr Position) = do
  mem <- stateArray <$> get
  pure $ mem ! (fromInteger addr)
fetch (Value val Immediate) = pure (toInteger val)

update :: (Integer, Integer) -> Computer ()
update (dest, value) = do
  state <- get
  let newArray = (stateArray state) // [(fromInteger dest, value)]
  put (state { stateArray = newArray })

setPC :: Integer -> Computer ()
setPC newPC = do
  state <- get
  put (state { statePC = fromInteger newPC })

incPC :: Int -> Computer ()
incPC increment = do
  state <- get
  let newPC = (statePC state) + increment
  put (state { statePC = newPC })

consume :: Computer Integer
consume = do
  state <- get
  let ioIn = stateInput state
  put (state { stateInput = tail ioIn })
  pure (head ioIn)

write :: Integer -> Computer ()
write value = do
  state <- get
  put (state { stateOutput = value : stateOutput state })

incRB :: Integer -> Computer ()
incRB increment = do
  state <- get
  let newRB = (stateRB state) + fromInteger increment
  put (state { stateRB = newRB })

isRunning :: ComputerState -> Bool
isRunning state = status state == Running

-- | Parsing the instructions

parseContents :: String -> Array
parseContents str = fromList $ read ("[" <> init str <> "]")

parseInstr :: Memory -> Instr
parseInstr (mem, pc) =
  let opInt = show (mem ! pc)
      (rest, op) = splitAt (length opInt - 2) opInt
      modes = parseModes rest
  in parseOp (read op) modes (mem, pc)

parseMode :: Char -> Mode
parseMode '0' = Position
parseMode '1' = Immediate
parseMode '2' = Relative
parseMode c   = error ("Mode \"" <> [c] <> "\" failed to parse")

parseModes :: String -> Modes
parseModes          []  = (   Position,    Position, Position)
parseModes       (x:[]) = (parseMode x,    Position, Position)
parseModes     (y:x:[]) = (parseMode x, parseMode y, Position)
parseModes ('2':y:x:[]) = (parseMode x, parseMode y, Relative)
parseModes modes =
  error ("Modes \"" <> modes <> "\" failed to parse (string is likely too long)")

parseOp :: Int -> Modes -> Memory -> Instr
parseOp = \case
  1  -> parseAdd
  2  -> parseMul
  3  -> parseInput
  4  -> parseOutput
  5  -> parseJumpIfTrue
  6  -> parseJumpIfFalse
  7  -> parseLessThan
  8  -> parseEquals
  9  -> parseRB
  99 -> (const . const) Halt
  o  -> error ("Op \"" <> show o <> "\" failed to parse")

parseAdd :: Modes -> Memory -> Instr
parseAdd (m1, m2, m3) (mem, pc) =
  let i1   = mem ! (pc + 1)
      i2   = mem ! (pc + 2)
      dest = mem ! (pc + 3)
  in Add (Value i1 m1) (Value i2 m2) (Value dest m3)

parseMul :: Modes -> Memory -> Instr
parseMul (m1, m2, m3) (mem, pc) =
  let i1   = mem ! (pc + 1)
      i2   = mem ! (pc + 2)
      dest = mem ! (pc + 3)
  in Mul (Value i1 m1) (Value i2 m2) (Value dest m3)

parseInput :: Modes -> Memory -> Instr
parseInput (m1, _, _) (mem, pc) =
  let dest = mem ! (pc + 1)
  in Input (Value dest m1)

parseOutput :: Modes -> Memory -> Instr
parseOutput (m1, _, _) (mem, pc) = Output (Value (mem ! (pc + 1)) m1)

parseJumpIfTrue :: Modes -> Memory -> Instr
parseJumpIfTrue (m1, m2, _) (mem, pc) =
  let cond = mem ! (pc + 1)
      dest = mem ! (pc + 2)
  in JumpIfTrue (Value cond m1) (Value dest m2)

parseJumpIfFalse :: Modes -> Memory -> Instr
parseJumpIfFalse (m1, m2, _) (mem, pc) =
  let cond = mem ! (pc + 1)
      dest = mem ! (pc + 2)
  in JumpIfFalse (Value cond m1) (Value dest m2)

parseLessThan :: Modes -> Memory -> Instr
parseLessThan (m1, m2, m3) (mem, pc) =
  let i1   = mem ! (pc + 1)
      i2   = mem ! (pc + 2)
      dest = mem ! (pc + 3)
  in LessThan (Value i1 m1) (Value i2 m2) (Value dest m3)

parseEquals :: Modes -> Memory -> Instr
parseEquals (m1, m2, m3) (mem, pc) =
  let i1   = mem ! (pc + 1)
      i2   = mem ! (pc + 2)
      dest = mem ! (pc + 3)
  in Equals (Value i1 m1) (Value i2 m2) (Value dest m3)

parseRB :: Modes -> Memory -> Instr
parseRB (m1, _, _) (mem, pc) =
  let v = mem ! (pc + 1)
  in RB (Value v m1)

-- | Running the program

fixState :: Computation -> ComputerState -> ComputerState
fixState computation current =
  let next = execState computation current
  in if isRunning next then fixState computation next else next

run :: ComputerState -> ComputerState
run = fixState compute

compute :: Computation
compute = fmap parseInstr memory >>= \case
  Add v1 v2 out    -> evalAdd v1 v2 out
  Mul v1 v2 out    -> evalMul v1 v2 out
  Input dest       -> evalInput dest
  Output val       -> evalOutput val
  JumpIfTrue c d   -> evalJumpIfTrue c d
  JumpIfFalse c d  -> evalJumpIfFalse c d
  LessThan v1 v2 o -> evalLessThan v1 v2 o
  Equals v1 v2 o   -> evalEquals v1 v2 o
  RB val           -> evalRB val
  Halt             -> evalHalt

evalAdd :: Value -> Value -> Value -> Computation
evalAdd v1 v2 v3 = do
  i1 <- fetch v1
  i2 <- fetch v2
  dest <- fetch v3
  update (dest, i1 + i2)
  incPC 4

evalMul :: Value -> Value -> Value -> Computation
evalMul v1 v2 v3 = do
  i1 <- fetch v1
  i2 <- fetch v2
  dest <- fetch v3
  update (dest, i1 * i2)
  incPC 4

evalInput :: Value -> Computation
evalInput value = do
  dest <- fetch value
  input <- consume
  update (dest, input)
  incPC 2

evalOutput :: Value -> Computation
evalOutput val = do
  output <- fetch val
  write output
  incPC 2

evalJumpIfTrue :: Value -> Value -> Computation
evalJumpIfTrue v1 v2 = do
  cond <- fetch v1
  dest <- fetch v2
  if cond /= 0
  then setPC dest
  else incPC 3

evalJumpIfFalse :: Value -> Value -> Computation
evalJumpIfFalse v1 v2 = do
  cond <- fetch v1
  dest <- fetch v2
  if cond == 0
  then setPC dest
  else incPC 3

evalLessThan :: Value -> Value -> Value -> Computation
evalLessThan v1 v2 v3 = do
  i1 <- fetch v1
  i2 <- fetch v2
  dest <- fetch v3
  update (dest, if i1 < i2 then 1 else 0)
  incPC 4

evalEquals :: Value -> Value -> Value -> Computation
evalEquals v1 v2 v3 = do
  i1 <- fetch v1
  i2 <- fetch v2
  dest <- fetch v3
  update (dest, if i1 == i2 then 1 else 0)
  incPC 4

evalRB :: Value -> Computation
evalRB val = do
  offset <- fetch val
  incRB offset
  incPC 2

evalHalt :: Computation
evalHalt = do
  state <- get
  put (state { status = Halted })

-- * Main

getInput :: IO Array
getInput = parseContents <$> readFile "input"

memorySize :: Int
memorySize = 100000

main :: IO ()
main = do
  mem <- (<> replicate memorySize 0) <$> getInput
  let initComputerState = ComputerState mem 0 [1] [] 0 Running
  print (reverse . stateOutput $ run initComputerState)
  

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad ((<=<))
import Data.Function ((&))
import Data.List (find, splitAt, foldl', permutations)
import Data.Vector (Vector, fromList, (!), (//))

data Mode
  = Position
  | Immediate
  deriving Show

data Value = Value Int Mode
  deriving Show

data Instr
  = Add Value Value Int
  | Mul Value Value Int
  | Input Int
  | Output Value
  | JumpIfTrue Value Value
  | JumpIfFalse Value Value
  | LessThan Value Value Int
  | Equals Value Value Int
  | Halt

type Modes = (Mode, Mode)
type Array = Vector Int
type PC = Int
type Memory = (Array, PC)

type IOBuffer = [Int]
data State = State
  { stateArray  :: Array
  , statePC     :: PC
  , stateInput  :: IOBuffer
  , stateOutput :: IOBuffer
  }

fetch :: Value -> Array -> Int
fetch (Value addr Position) mem = mem ! addr
fetch (Value val Immediate) _ = val

stateMemory :: State -> Memory
stateMemory state =
  (stateArray state, statePC state)

-- | Parsing the instructions

parseContents :: String -> [Int]
parseContents str = read ("[" <> init str <> "]")

parseInstr :: Memory -> Instr
parseInstr (mem, pc) =
  let
      opInt = show (mem ! pc)
      (rest, op) = splitAt (length opInt - 2) opInt
      modes = parseModes rest
  in parseOp (read op) modes (mem, pc)

parseOp :: Int -> Modes -> Memory -> Instr
parseOp = \case
  1  -> parseAdd
  2  -> parseMul
  3  -> const parseInput
  4  -> parseOutput
  5  -> parseJumpIfTrue
  6  -> parseJumpIfFalse
  7  -> parseLessThan
  8  -> parseEquals
  99 -> (const . const) Halt
  o  -> error ("Op \"" <> show o <> "\" failed to parse (it is not one of 1, 2, 3, 4, 5, 6, 7, 8, 99)")

parseAdd :: Modes -> Memory -> Instr
parseAdd (m1, m2) (mem, pc) =
  let i1   = mem ! (pc + 1)
      i2   = mem ! (pc + 2)
      dest = mem ! (pc + 3)
  in Add (Value i1 m1) (Value i2 m2) dest

parseMul :: Modes -> Memory -> Instr
parseMul (m1, m2) (mem, pc) =
  let i1   = mem ! (pc + 1)
      i2   = mem ! (pc + 2)
      dest = mem ! (pc + 3)
  in Mul (Value i1 m1) (Value i2 m2) dest

parseInput :: Memory -> Instr
parseInput (mem, pc) = Input (mem ! (pc + 1))

parseOutput :: Modes -> Memory -> Instr
parseOutput (m1, _) (mem, pc) = Output (Value (mem ! (pc + 1)) m1)

parseJumpIfTrue :: Modes -> Memory -> Instr
parseJumpIfTrue (m1, m2) (mem, pc) =
  let cond = mem ! (pc + 1)
      dest = mem ! (pc + 2)
  in JumpIfTrue (Value cond m1) (Value dest m2)

parseJumpIfFalse :: Modes -> Memory -> Instr
parseJumpIfFalse (m1, m2) (mem, pc) =
  let cond = mem ! (pc + 1)
      dest = mem ! (pc + 2)
  in JumpIfFalse (Value cond m1) (Value dest m2)

parseLessThan :: Modes -> Memory -> Instr
parseLessThan (m1, m2) (mem, pc) =
  let i1   = mem ! (pc + 1)
      i2   = mem ! (pc + 2)
      dest = mem ! (pc + 3)
  in LessThan (Value i1 m1) (Value i2 m2) dest

parseEquals :: Modes -> Memory -> Instr
parseEquals (m1, m2) (mem, pc) =
  let i1   = mem ! (pc + 1)
      i2   = mem ! (pc + 2)
      dest = mem ! (pc + 3)
  in Equals (Value i1 m1) (Value i2 m2) dest

parseMode :: Char -> Mode
parseMode '0' = Position
parseMode '1' = Immediate
parseMode c   = error ("Mode \"" <> [c] <> "\" failed to parse (is not one of '0', '1')")

parseModes :: String -> (Mode, Mode)
parseModes []       = (   Position,    Position)
parseModes (x:[])   = (parseMode x,    Position)
parseModes (x:y:[]) = (parseMode y, parseMode x)
parseModes modes =
  error ("Modes \"" <> modes <> "\" failed to parse (string is likely too long)")

-- | Running the program

compute :: State -> State
compute current =
  case eval current of
    Just new -> compute new
    Nothing  -> current

eval :: State -> Maybe State
eval state = go (parseInstr $ stateMemory state) state
  where
    go :: Instr -> State -> Maybe State
    go = \case
      Add v1 v2 out    -> evalAdd v1 v2 out
      Mul v1 v2 out    -> evalMul v1 v2 out
      Input dest       -> evalInput dest
      Output val       -> evalOutput val
      JumpIfTrue c d   -> evalJumpIfTrue c d
      JumpIfFalse c d  -> evalJumpIfFalse c d
      LessThan v1 v2 o -> evalLessThan v1 v2 o
      Equals v1 v2 o   -> evalEquals v1 v2 o
      Halt             -> const Nothing

evalAdd :: Value -> Value -> Int -> State -> Maybe State
evalAdd v1 v2 dest (State mem pc ioIn ioOut) =
  let i1 = fetch v1 mem
      i2 = fetch v2 mem
      newMem = mem // [(dest, i1 + i2)]
  in Just (State newMem (pc + 4) ioIn ioOut)

evalMul :: Value -> Value -> Int -> State -> Maybe State
evalMul v1 v2 dest (State mem pc ioIn ioOut) =
  let i1 = fetch v1 mem
      i2 = fetch v2 mem
      newMem = mem // [(dest, i1 * i2)]
  in Just (State newMem (pc + 4) ioIn ioOut)

evalInput :: Int -> State -> Maybe State
evalInput dest (State mem pc ioIn ioOut) =
  let newMem = mem // [(dest, head ioIn)]
  in Just (State newMem (pc + 2) (tail ioIn) ioOut)

evalOutput :: Value -> State -> Maybe State
evalOutput val (State mem pc ioIn ioOut) =
  Just (State mem (pc + 2) ioIn (fetch val mem : ioOut))

evalJumpIfTrue :: Value -> Value -> State -> Maybe State
evalJumpIfTrue c d (State mem pc ioIn ioOut) =
  let cond = fetch c mem
      dest = fetch d mem
      trueState  = State mem dest ioIn ioOut
      falseState = State mem (pc + 3) ioIn ioOut
  in Just (if cond /= 0 then trueState else falseState)

evalJumpIfFalse :: Value -> Value -> State -> Maybe State
evalJumpIfFalse c d (State mem pc ioIn ioOut) =
  let cond = fetch c mem
      dest = fetch d mem
      trueState  = State mem dest ioIn ioOut
      falseState = State mem (pc + 3) ioIn ioOut
  in Just (if cond == 0 then trueState else falseState)

evalLessThan :: Value -> Value -> Int -> State -> Maybe State
evalLessThan v1 v2 dest (State mem pc ioIn ioOut) =
  let i1 = fetch v1 mem
      i2 = fetch v2 mem
      newMem = mem // [(dest, if i1 < i2 then 1 else 0)]
  in Just (State newMem (pc + 4) ioIn ioOut)

evalEquals :: Value -> Value -> Int -> State -> Maybe State
evalEquals v1 v2 dest (State mem pc ioIn ioOut) =
  let i1 = fetch v1 mem
      i2 = fetch v2 mem
      newMem = mem // [(dest, if i1 == i2 then 1 else 0)]
  in Just (State newMem (pc + 4) ioIn ioOut)

-- * Main

getInput :: IO Array
getInput = fromList <$> parseContents <$> readFile "input"

main :: IO ()
main = pure ()

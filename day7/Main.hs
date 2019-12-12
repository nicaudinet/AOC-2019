{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad ((<=<))
import Data.List (find, splitAt)
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
type Memory = Vector Int
type PC = Int
type State = (Memory, PC)

-- | Parsing the instructions

parseContents :: String -> [Int]
parseContents str = read ("[" <> init str <> "]")

parseInstr :: State -> Instr
parseInstr (mem, pc) =
  let 
      opInt = show (mem ! pc)
      (rest, op) = splitAt (length opInt - 2) opInt
      modes = parseModes rest
  in parseOp (read op) modes (mem, pc)

parseOp :: Int -> Modes -> State -> Instr
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

parseAdd :: Modes -> State -> Instr
parseAdd (m1, m2) (mem, pc) =
  let i1   = mem ! (pc + 1)
      i2   = mem ! (pc + 2)
      dest = mem ! (pc + 3)
  in Add (Value i1 m1) (Value i2 m2) dest

parseMul :: Modes -> State -> Instr
parseMul (m1, m2) (mem, pc) =
  let i1   = mem ! (pc + 1)
      i2   = mem ! (pc + 2)
      dest = mem ! (pc + 3)
  in Mul (Value i1 m1) (Value i2 m2) dest

parseInput :: State -> Instr
parseInput (mem, pc) = Input (mem ! (pc + 1))

parseOutput :: Modes -> State -> Instr
parseOutput (m1, _) (mem, pc) = Output (Value (mem ! (pc + 1)) m1)

parseJumpIfTrue :: Modes -> State -> Instr
parseJumpIfTrue (m1, m2) (mem, pc) =
  let cond = mem ! (pc + 1)
      dest = mem ! (pc + 2)
  in JumpIfTrue (Value cond m1) (Value dest m2)

parseJumpIfFalse :: Modes -> State -> Instr
parseJumpIfFalse (m1, m2) (mem, pc) =
  let cond = mem ! (pc + 1)
      dest = mem ! (pc + 2)
  in JumpIfFalse (Value cond m1) (Value dest m2)

parseLessThan :: Modes -> State -> Instr
parseLessThan (m1, m2) (mem, pc) =
  let i1   = mem ! (pc + 1)
      i2   = mem ! (pc + 2)
      dest = mem ! (pc + 3)
  in LessThan (Value i1 m1) (Value i2 m2) dest

parseEquals :: Modes -> State -> Instr
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
parseModes modes    = error ("Modes \"" <> modes <> "\" failed to parse (string is likely too long)")

-- | Running the program

compute :: State -> IO Memory
compute (mem, pc) =
  eval (mem, pc) >>= \case
    Just newState -> compute newState
    Nothing       -> pure mem

eval :: State -> IO (Maybe State)
eval (mem, pc) = operation (mem, pc)
  where
    operation :: State -> IO (Maybe State)
    operation =
      case parseInstr (mem, pc) of
        Add v1 v2 out    -> evalAdd v1 v2 out
        Mul v1 v2 out    -> evalMul v1 v2 out
        Input dest       -> evalInput dest
        Output val       -> evalOutput val
        JumpIfTrue c d   -> evalJumpIfTrue c d
        JumpIfFalse c d  -> evalJumpIfFalse c d
        LessThan v1 v2 o -> evalLessThan v1 v2 o
        Equals v1 v2 o   -> evalEquals v1 v2 o
        Halt             -> const evalHalt

fetch :: Value -> Memory -> Int
fetch (Value addr Position) mem = mem ! addr
fetch (Value val Immediate) _ = val

evalAdd :: Value -> Value -> Int -> State -> IO (Maybe State)
evalAdd v1 v2 dest (mem, pc) = do
  let i1 = fetch v1 mem
      i2 = fetch v2 mem
      newMem = mem // [(dest, i1 + i2)]
  pure $ Just (newMem, pc + 4)
    
evalMul :: Value -> Value -> Int -> State -> IO (Maybe State)
evalMul v1 v2 dest (mem, pc) = do
  let i1 = fetch v1 mem
      i2 = fetch v2 mem
      newMem = mem // [(dest, i1 * i2)]
  pure $ Just (newMem, pc + 4)

evalInput :: Int -> State -> IO (Maybe State)
evalInput dest (mem, pc) =  do
  input <- read <$> getLine
  let newMem = mem // [(dest, input)]
  pure $ Just (newMem, pc + 2)

evalOutput :: Value -> State -> IO (Maybe State)
evalOutput val (mem, pc) = do
  print (fetch val mem)
  pure $ Just (mem, pc + 2)

evalJumpIfTrue :: Value -> Value -> State -> IO (Maybe State)
evalJumpIfTrue c d (mem, pc) = do
  let cond = fetch c mem
      dest = fetch d mem
  pure $ Just (if cond /= 0 then (mem, dest) else (mem, pc + 3))

evalJumpIfFalse :: Value -> Value -> State -> IO (Maybe State)
evalJumpIfFalse c d (mem, pc) = do
  let cond = fetch c mem
      dest = fetch d mem
  pure $ Just (if cond == 0 then (mem, dest) else (mem, pc + 3))

evalLessThan :: Value -> Value -> Int -> State -> IO (Maybe State)
evalLessThan v1 v2 dest (mem, pc) = do
  let i1 = fetch v1 mem
      i2 = fetch v2 mem
      newMem = mem // [(dest, if i1 < i2 then 1 else 0)]
  pure $ Just (newMem, pc + 4)

evalEquals :: Value -> Value -> Int -> State -> IO (Maybe State)
evalEquals v1 v2 dest (mem, pc) = do
  let i1 = fetch v1 mem
      i2 = fetch v2 mem
      newMem = mem // [(dest, if i1 == i2 then 1 else 0)]
  pure $ Just (newMem, pc + 4)

evalHalt :: IO (Maybe State)
evalHalt = pure Nothing

main :: IO ()
main = do
  contents <- parseContents <$> readFile "input"
  let mem = fromList contents
  compute (mem, 0)
  pure ()

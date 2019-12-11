{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad ((<=<))
import Data.List (find, splitAt)
import Data.Vector (Vector, fromList, (!), (//))
import GHC.Stack

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

type Memory = Vector Int
type PC = Int
type Modes = (Mode, Mode)

-- | Parsing the instructions

parseContents :: HasCallStack => String -> [Int]
parseContents str = read ("[" <> init str <> "]")

parseInstr :: HasCallStack => Memory -> PC -> Instr
parseInstr mem pc = 
  let 
      opInt = show (mem ! pc)
      (rest, op) = splitAt (length opInt - 2) opInt
      modes = parseModes rest
  in  
    case read op of
      1  -> parseAdd modes mem pc
      2  -> parseMul modes mem pc
      3  -> parseInput mem pc
      4  -> parseOutput modes mem pc
      5  -> parseJumpIfTrue modes mem pc
      6  -> parseJumpIfFalse modes mem pc
      7  -> parseLessThan modes mem pc
      8  -> parseEquals modes mem pc
      99 -> Halt
      o  -> error ("Op \"" <> op <> "\" failed to parse (it is not one of 1, 2, 3, 4, 5, 6, 7, 8, 99)")

parseAdd :: Modes -> Memory -> PC -> Instr
parseAdd (m1, m2) mem pc =
  let i1   = mem ! (pc + 1)
      i2   = mem ! (pc + 2)
      dest = mem ! (pc + 3)
  in Add (Value i1 m1) (Value i2 m2) dest

parseMul :: Modes -> Memory -> PC -> Instr
parseMul (m1, m2) mem pc =
  let i1   = mem ! (pc + 1)
      i2   = mem ! (pc + 2)
      dest = mem ! (pc + 3)
  in Mul (Value i1 m1) (Value i2 m2) dest

parseInput :: Memory -> PC -> Instr
parseInput mem pc = Input (mem ! (pc + 1))

parseOutput :: Modes -> Memory -> PC -> Instr
parseOutput (m1, _) mem pc = Output (Value (mem ! (pc + 1)) m1)

parseJumpIfTrue :: Modes -> Memory -> PC -> Instr
parseJumpIfTrue (m1, m2) mem pc =
  let cond = mem ! (pc + 1)
      dest = mem ! (pc + 2)
  in JumpIfTrue (Value cond m1) (Value dest m2)

parseJumpIfFalse :: Modes -> Memory -> PC -> Instr
parseJumpIfFalse (m1, m2) mem pc =
  let cond = mem ! (pc + 1)
      dest = mem ! (pc + 2)
  in JumpIfFalse (Value cond m1) (Value dest m2)

parseLessThan :: Modes -> Memory -> PC -> Instr
parseLessThan (m1, m2) mem pc =
  let i1   = mem ! (pc + 1)
      i2   = mem ! (pc + 2)
      dest = mem ! (pc + 3)
  in LessThan (Value i1 m1) (Value i2 m2) dest

parseEquals :: Modes -> Memory -> PC -> Instr
parseEquals (m1, m2) mem pc =
  let i1   = mem ! (pc + 1)
      i2   = mem ! (pc + 2)
      dest = mem ! (pc + 3)
  in Equals (Value i1 m1) (Value i2 m2) dest

parseMode :: HasCallStack => Char -> Mode
parseMode '0' = Position
parseMode '1' = Immediate
parseMode c   = error ("Mode \"" <> [c] <> "\" failed to parse (is not one of '0', '1')")

parseModes :: HasCallStack => String -> (Mode, Mode)
parseModes []       = (   Position,    Position)
parseModes (x:[])   = (parseMode x,    Position)
parseModes (x:y:[]) = (parseMode y, parseMode x)
parseModes modes    = error ("Modes \"" <> modes <> "\" failed to parse (string is likely too long)")

-- | Running the program

compute :: HasCallStack => PC -> Memory -> IO Memory
compute pc mem =
  eval mem pc >>= \case
    (pc', Just mem') -> compute pc' mem'
    (_, Nothing)     -> pure mem

eval :: HasCallStack => Memory -> PC -> IO (PC, Maybe Memory)
eval mem pc =
  case parseInstr mem pc of
    Add v1 v2 out    -> add v1 v2 out mem pc
    Mul v1 v2 out    -> mul v1 v2 out mem pc
    Input dest       -> input dest mem pc
    Output val       -> output val mem pc
    JumpIfTrue c d   -> jumpIfTrue c d mem pc
    JumpIfFalse c d  -> jumpIfFalse c d mem pc
    LessThan v1 v2 o -> lessThan v1 v2 o mem pc
    Equals v1 v2 o   -> equals v1 v2 o mem pc
    Halt             -> halt

fetch :: HasCallStack => Value -> Memory -> Int
fetch (Value addr Position) mem = mem ! addr
fetch (Value val Immediate) _ = val

add :: HasCallStack => Value -> Value -> Int -> Memory -> PC -> IO (PC, Maybe Memory)
add v1 v2 dest mem pc = do
  let i1 = fetch v1 mem
      i2 = fetch v2 mem
      mem' = Just (mem // [(dest, i1 + i2)])
  pure (pc + 4, mem')
    
mul :: HasCallStack => Value -> Value -> Int -> Memory -> PC -> IO (PC, Maybe Memory)
mul v1 v2 dest mem pc = do
  let i1 = fetch v1 mem
      i2 = fetch v2 mem
      mem' = Just (mem // [(dest, i1 * i2)])
  pure (pc + 4, mem')

input :: HasCallStack => Int -> Memory -> PC -> IO (PC, Maybe Memory)
input dest mem pc =  do
  input <- read <$> getLine
  let mem' = Just (mem // [(dest, input)])
  pure (pc + 2, mem')

output :: HasCallStack => Value -> Memory -> PC -> IO (PC, Maybe Memory)
output val mem pc = do
  print (fetch val mem)
  let mem' = Just mem
  pure (pc + 2, mem')

jumpIfTrue :: Value -> Value -> Memory -> PC -> IO (PC, Maybe Memory)
jumpIfTrue c d mem pc = do
  let cond = fetch c mem
      dest = fetch d mem
  pure $ if cond /= 0 then (dest, Just mem) else (pc + 3, Just mem)

jumpIfFalse :: Value -> Value -> Memory -> PC -> IO (PC, Maybe Memory)
jumpIfFalse c d mem pc = do
  let cond = fetch c mem
      dest = fetch d mem
  pure $ if cond == 0 then (dest, Just mem) else (pc + 3, Just mem)

lessThan :: Value -> Value -> Int -> Memory -> PC -> IO (PC, Maybe Memory)
lessThan v1 v2 dest mem pc = do
  let i1 = fetch v1 mem
      i2 = fetch v2 mem
      mem' = Just (mem // [(dest, if i1 < i2 then 1 else 0)])
  pure (pc + 4, mem')

equals :: Value -> Value -> Int -> Memory -> PC -> IO (PC, Maybe Memory)
equals v1 v2 dest mem pc = do
  let i1 = fetch v1 mem
      i2 = fetch v2 mem
      mem' = Just (mem // [(dest, if i1 == i2 then 1 else 0)])
  pure (pc + 4, mem')

halt :: HasCallStack => IO (PC, Maybe Memory)
halt = pure (error "Tried to call PC after Halt instruction", Nothing)

main :: HasCallStack => IO ()
main = do
  contents <- parseContents <$> readFile "input"
  let mem = fromList contents
  compute 0 mem
  pure ()

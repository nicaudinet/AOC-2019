module Main where

import Control.Monad ((<=<))
import Data.List (find, splitAt)
import Data.Vector (Vector, fromList, (!), (//))
import Debug.Trace
import GHC.Stack

type Memory = Vector Int
type PC = Int

data Mode
  = Position
  | Immediate

data Value = Value Int Mode
data Instr
  = Add Value Value Int
  | Mul Value Value Int
  | Input Int
  | Output Value
  | Halt

-- | Parsing the instructions

parseContents :: HasCallStack => String -> [Int]
parseContents str = read ("[" <> init str <> "]")

parseInstr :: HasCallStack => Memory -> PC -> (Instr, PC)
parseInstr mem pc = 
  let 
      opInt = show (mem ! pc)
      (modes, op) = splitAt (length opInt - 2) opInt
  in  
    case read op of
      1  -> parseAdd    modes mem pc
      2  -> parseMul    modes mem pc
      3  -> parseInput        mem pc
      4  -> parseOutput modes mem pc
      99 -> (Halt, error "PC was called after Halt instruction")
      o  -> error ("Op \"" <> op <> "\" failed to parse (it is not one of 1, 2, 3, 4, 99)")

parseAdd :: String -> Memory -> PC -> (Instr, PC)
parseAdd modes mem pc =
  let (m1, m2) = parseModes modes
      i1  = mem ! (pc + 1)
      i2  = mem ! (pc + 2)
      out = mem ! (pc + 3)
      instr = Add (Value i1 m1) (Value i2 m2) out
  in (instr, pc + 4)

parseMul :: String -> Memory -> PC -> (Instr, PC)
parseMul modes mem pc =
  let (m1, m2) = parseModes modes
      i1  = mem ! (pc + 1)
      i2  = mem ! (pc + 2)
      out = mem ! (pc + 3)
      instr = Mul (Value i1 m1) (Value i2 m2) out
  in (instr, pc + 4)

parseInput :: Memory -> PC -> (Instr, PC)
parseInput mem pc =
  let dest = mem ! (pc + 1)
      instr = Input dest
  in (instr, pc + 2)

parseOutput :: String -> Memory -> PC -> (Instr, PC)
parseOutput modes mem pc =
  let (m1, _) = parseModes modes
      toPrint = mem ! (pc + 1)
      instr = Output (Value toPrint m1)
  in (instr, pc + 2)

parseMode :: HasCallStack => Char -> Mode
parseMode '0' = Position
parseMode '1' = Immediate
parseMode c   = error ("Mode \"" <> [c] <> "\" failed to parse (is not one of '0', '1')")

parseModes :: HasCallStack => String -> (Mode, Mode)
parseModes []         = (   Position,    Position)
parseModes (x:[])     = (parseMode x,    Position)
parseModes (x:y:[])   = (parseMode x, parseMode y)
parseModes modes      = error ("Modes \"" <> modes <> "\" failed to parse (string is likely too long)")

-- | Running the program

compute :: HasCallStack => PC -> Memory -> IO Memory
compute pc mem = do
  let (instr, pc') = parseInstr mem pc
  compute pc' =<< eval instr mem

eval :: HasCallStack => Instr -> Memory -> IO Memory
eval instr =
  case instr of
    Add v1 v2 out -> add v1 v2 out
    Mul v1 v2 out -> mul v1 v2 out
    Input dest    -> input dest
    Output val    -> output val
    Halt          -> halt

fetch :: HasCallStack => Value -> Memory -> Int
fetch (Value addr Position) mem = trace ("fetch: " <> show addr) $ mem ! addr
fetch (Value val Immediate) _ = val

add :: HasCallStack => Value -> Value -> Int -> Memory -> IO Memory
add v1 v2 out mem = do
  let i1 = fetch v1 mem
      i2 = fetch v2 mem
  pure (mem // [(out, i1 + i2)])
    

mul :: HasCallStack => Value -> Value -> Int -> Memory -> IO Memory
mul v1 v2 out mem = do
  let i1 = fetch v1 mem
      i2 = fetch v2 mem
  pure (mem // [(out, i1 * i2)])

input :: HasCallStack => Int -> Memory -> IO Memory
input dest mem = do
  input <- read <$> getLine
  pure (mem // [(dest, input)])

output :: HasCallStack => Value -> Memory -> IO Memory
output val mem = do
  print (fetch val mem)
  pure mem

halt :: HasCallStack => Memory -> IO Memory
halt = pure

main :: HasCallStack => IO ()
main = do
  contents <- parseContents <$> readFile "input"
  putStr "Length of contents: "
  print (length contents)
  print contents
  let mem = fromList contents
  compute 0 mem
  print "Done"

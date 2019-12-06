module Main where

import Control.Monad ((<=<))
import Data.List (find)
import Data.Vector (Vector, fromList, (!), (//))

type Program = [Instr]
type Memory = Vector Int

data Op
  = Add
  | Mul
  | Input
  | Output
  | Halt

data Mode
  = Position
  | Immediate
  | HaltMode

data Value = Value Int Mode
data Instr = Instr Op Value Value Value

parseInput :: String -> [Int]
parseInput str = read ("[" <> init str <> "]")

parseInstrs :: [Int] -> Program
parseInstrs (x : mem) = instr : parseInstrs rest
  where
    (op, m1, m2, m3) = parseOp x
    (v1, v2, v3, rest) =
      case op of
        Add    -> op3 mem
        Mul    -> op3 mem
        Input  -> op1 mem
        Output -> op1 mem
        Halt   -> op0 mem
    instr = Instr op (Value v1 m1) (Value v2 m2) (Value v3 m3)

nan :: Int
nan = error "The impossible happend: This operation should not have this argument."

op0 :: [Int] -> (Int, Int, Int, [Int])
op0 mem = (nan, nan, nan, mem)

op1 :: [Int] -> (Int, Int, Int, [Int])
op1 (x : mem) = (x, nan, nan, mem)

op3 :: [Int] -> (Int, Int, Int, [Int])
op3 (i1 : i2 : o : mem) = (i1, i2, o, mem)

parseOp :: Int -> (Op, Mode, Mode, Mode)
parseOp = undefined

compute :: Program -> Memory -> IO Memory
compute (instr : rest) mem = compute rest =<< eval instr mem

eval :: Instr -> Memory -> IO Memory
eval (Instr op v1 v2 v3) =
  case op of
    Add    -> add v1 v2 v3
    Mul    -> mul v1 v2 v3
    Input  -> input v1
    Output -> output v2
    Halt   -> halt

add :: Value -> Value -> Value -> Memory -> IO Memory
add i1 i2 o mem = undefined

mul :: Value -> Value -> Value -> Memory -> IO Memory
mul = undefined

input :: Value -> Memory -> IO Memory
input pc mem = undefined

output :: Value -> Memory -> IO Memory
output pc mem = undefined

halt :: Memory -> IO Memory
halt = pure

main :: IO ()
main = do
  contents <- parseInput <$> readFile "input"
  print "We got somewhere"
  let program = parseInstrs contents
      mem = fromList contents
  compute program mem
  print "Done"

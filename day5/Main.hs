module Main where

import Data.List (find)
import Data.Vector (Vector, fromList, (!), (//))

type MemoryVec = Vector Int
type PC = Int
data Memory = Memory MemoryVec PC

current :: Memory -> Int
current Memory mem pc = mem ! pc

parse :: String -> MemoryVec
parse str = fromList (read ("[" <> init str <> "]"))

run :: Memory -> IO Int
run mem = fmap (! 0) (compute mem) 

compute :: Memory -> IO MemoryVec
compute mem =
  case current mem of
    1 -> compute (pc + 4) (op3 (+) mem pc)
    2 -> compute (pc + 4) (op3 (*) mem pc)
    3 -> input mem pc
    4 -> output mem pc
    99 -> pure mem



op3 :: (Int -> Int -> Int) -> Memory -> PC -> Memory
op3 fn mem pc =
  let in1 = mem ! (pc + 1)
      in2 = mem ! (pc + 2)
      out = mem ! (pc + 3)
      res = fn (mem ! in1) (mem ! in2)
  in mem // [(out, res)]

main :: IO ()
main = do
  print "hello world"

module Main where

import Data.Vector (Vector, fromList, (!), (//))

type Memory = Vector Int
type PC = Int

compute :: PC -> Memory -> Memory
compute pc mem =
  case mem ! pc of
    1 -> compute (pc + 4) (op (+) mem pc)
    2 -> compute (pc + 4) (op (*) mem pc)
    99 -> mem

op :: (Int -> Int -> Int) -> Memory -> PC -> Memory
op fn mem pc =
  let in1 = mem ! (pc + 1)
      in2 = mem ! (pc + 2)
      out = mem ! (pc + 3)
      res = fn (mem ! in1) (mem ! in2)
  in mem // [(out, res)]

fix :: Memory -> Memory
fix mem = mem // [(1, 12), (2, 2)]

parse :: String -> Memory
parse str = fromList (read ("[" <> init str <> "]"))

main :: IO ()
main = do
  contents <- readFile "input"
  let program = fix (parse contents)
  print ((compute 0 program) ! 0)

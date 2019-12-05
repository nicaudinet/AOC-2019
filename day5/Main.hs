module Main where

import Data.List (find)
import Data.Vector (Vector, fromList, (!), (//))

type Memory = Vector Int
type PC = Int

run :: Memory -> Int
run mem = (compute 0 mem) ! 0

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

parse :: String -> Memory
parse str = fromList (read ("[" <> init str <> "]"))

setNoun :: Int -> Memory -> Memory
setNoun n mem = mem // [(1, n)]

setVerb :: Int -> Memory -> Memory
setVerb n mem = mem // [(2, n)]

fix :: Memory -> Memory
fix = setNoun 12 . setVerb 2

runFix :: Int -> Int -> Memory -> Int
runFix n v = run . setNoun n . setVerb v

search :: Memory -> (Int, Int)
search mem =
  let outputs = [ (n, v, runFix n v mem) | n <- [0..99], v <- [0..99] ]
      isResult (_, _, res) = res == 19690720
      Just (n, v, _) = find isResult outputs
  in (n, v)

main :: IO ()
main = do
  contents <- readFile "input"
  let program = parse contents
  print (run (fix program))
  let (n, v) = search program
  print (100 * n + v)

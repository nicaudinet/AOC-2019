module Main where

import Control.Monad ((<=<))
import Data.List (find)
import Data.Vector (Vector, fromList, (!), (//))

type Memory = Vector Int
type PC = Int

parse :: String -> Memory
parse str = fromList (read ("[" <> init str <> "]"))

run :: Memory -> IO Int
run mem = fmap (! 0) (compute 0 mem)

compute :: PC -> Memory -> IO Memory
compute pc mem =
  case mem ! pc of
    1  -> compute (pc + 4) (op3 (+) mem pc)
    2  -> compute (pc + 4) (op3 (*) mem pc)
    3  -> compute (pc + 2) =<< input pc mem
    4  -> compute (pc + 2) =<< output pc mem
    99 -> pure mem

input :: PC -> Memory -> IO Memory
input pc mem = do
  let address = mem ! (pc + 1)
  num <- read <$> getLine
  pure $ mem // [(address, num)]

output :: PC -> Memory -> IO Memory
output pc mem = do
  let address = mem ! (pc + 1)
  print (mem ! address)
  pure mem

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

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module IntCode where

import Prelude hiding (replicate)
import Control.Monad ((<=<))
import Control.Monad.State
import Data.Function ((&))
import Data.List (find, splitAt, foldl', permutations)
import Data.Vector (Vector, fromList, (!), (//), replicate)

data InMode
  = InPosition
  | InImmediate
  | InRelative
  deriving Show

data OutMode
  = OutPosition
  | OutRelative
  deriving Show

data InValue = InValue Integer InMode
  deriving Show

data OutValue = OutValue Integer OutMode
  deriving Show

data Instr
  = Add InValue InValue OutValue
  | Mul InValue InValue OutValue
  | Input OutValue
  | Output InValue
  | JumpIfTrue InValue InValue
  | JumpIfFalse InValue InValue
  | LessThan InValue InValue OutValue
  | Equals InValue InValue OutValue
  | RB InValue
  | Halt

data Status = Running | Halted | Reading
  deriving (Eq, Show)

type Modes = String
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

initState :: Array -> ComputerState
initState program =
  ComputerState (program <> replicate 1000 0) 0 [] [] 0 Running

stateMemory :: ComputerState -> Memory
stateMemory state =
  (stateArray state, statePC state)

fetchIn :: InValue -> Computer Integer
fetchIn (InValue addr InRelative) = do
  mem <- gets stateArray
  rb  <- gets stateRB
  pure $ mem ! (fromInteger addr + rb)
fetchIn (InValue addr InPosition) = do
  mem <- gets stateArray
  pure $ mem ! (fromInteger addr)
fetchIn (InValue val InImmediate) = pure (toInteger val)

fetchOut :: OutValue -> Computer Integer
fetchOut (OutValue dest OutPosition) = pure dest
fetchOut (OutValue dest OutRelative) = do
  rb <- toInteger <$> gets stateRB
  pure (dest + rb)

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

getInput :: Computer ()
getInput = do
  state <- get
  put (state { status = Reading })

-- * Parsing the instructions

parseContents :: String -> Array
parseContents str = fromList $ read ("[" <> init str <> "]")

parseInstr :: Memory -> Instr
parseInstr (mem, pc) =
  let opInt = show (mem ! pc)
      (modes, op) = splitAt (length opInt - 2) opInt
  in parseOp (read op) modes (mem, pc)

parseModeIn :: Char -> InMode
parseModeIn '0' = InPosition
parseModeIn '1' = InImmediate
parseModeIn '2' = InRelative
parseModeIn c   = error ("Mode \"" <> [c] <> "\" failed to parse")

parseOneModeIn :: String -> InMode
parseOneModeIn []    = InPosition
parseOneModeIn ['1'] = InImmediate
parseOneModeIn ['2'] = InRelative

parseOneModeOut :: String -> OutMode
parseOneModeOut []    = OutPosition
parseOneModeOut ['2'] = OutRelative

parseTwoModes :: String -> (InMode, InMode)
parseTwoModes      []  = (   InPosition,    InPosition)
parseTwoModes   (x:[]) = (parseModeIn x,    InPosition)
parseTwoModes (y:x:[]) = (parseModeIn x, parseModeIn y)

parseThreeModes :: String -> (InMode, InMode, OutMode)
parseThreeModes          []  = (   InPosition,    InPosition, OutPosition)
parseThreeModes       (x:[]) = (parseModeIn x,    InPosition, OutPosition)
parseThreeModes     (y:x:[]) = (parseModeIn x, parseModeIn y, OutPosition)
parseThreeModes ('2':y:x:[]) = (parseModeIn x, parseModeIn y, OutRelative)
parseThreeModes modes =
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
parseAdd modes (mem, pc) =
  let (m1, m2, m3) = parseThreeModes modes
      i1   = mem ! (pc + 1)
      i2   = mem ! (pc + 2)
      dest = mem ! (pc + 3)
  in Add (InValue i1 m1) (InValue i2 m2) (OutValue dest m3)

parseMul :: Modes -> Memory -> Instr
parseMul modes (mem, pc) =
  let (m1, m2, m3) = parseThreeModes modes
      i1   = mem ! (pc + 1)
      i2   = mem ! (pc + 2)
      dest = mem ! (pc + 3)
  in Mul (InValue i1 m1) (InValue i2 m2) (OutValue dest m3)

parseInput :: Modes -> Memory -> Instr
parseInput mode (mem, pc) =
  let m1 = parseOneModeOut mode
      dest = mem ! (pc + 1)
  in Input (OutValue dest m1)

parseOutput :: Modes -> Memory -> Instr
parseOutput mode (mem, pc) =
  let m1 = parseOneModeIn mode
  in Output (InValue (mem ! (pc + 1)) m1)

parseJumpIfTrue :: Modes -> Memory -> Instr
parseJumpIfTrue modes (mem, pc) =
  let (m1, m2) = parseTwoModes modes
      cond = mem ! (pc + 1)
      dest = mem ! (pc + 2)
  in JumpIfTrue (InValue cond m1) (InValue dest m2)

parseJumpIfFalse :: Modes -> Memory -> Instr
parseJumpIfFalse modes (mem, pc) =
  let (m1, m2) = parseTwoModes modes
      cond = mem ! (pc + 1)
      dest = mem ! (pc + 2)
  in JumpIfFalse (InValue cond m1) (InValue dest m2)

parseLessThan :: Modes -> Memory -> Instr
parseLessThan modes (mem, pc) =
  let (m1, m2, m3) = parseThreeModes modes
      i1   = mem ! (pc + 1)
      i2   = mem ! (pc + 2)
      dest = mem ! (pc + 3)
  in LessThan (InValue i1 m1) (InValue i2 m2) (OutValue dest m3)

parseEquals :: Modes -> Memory -> Instr
parseEquals modes (mem, pc) =
  let (m1, m2, m3) = parseThreeModes modes
      i1   = mem ! (pc + 1)
      i2   = mem ! (pc + 2)
      dest = mem ! (pc + 3)
  in Equals (InValue i1 m1) (InValue i2 m2) (OutValue dest m3)

parseRB :: Modes -> Memory -> Instr
parseRB mode (mem, pc) =
  let m1 = parseOneModeIn mode
      v = mem ! (pc + 1)
  in RB (InValue v m1)

-- | Running the program

run :: ComputerState -> ComputerState
run current =
  let next = execState compute current
  in if not (isRunning next) then next else run next

runHalt :: ComputerState -> ComputerState
runHalt current =
  let next = execState compute current
  in if status next == Halted then next else run next

compute :: Computation
compute = fmap parseInstr (gets stateMemory) >>= \case
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

evalAdd :: InValue -> InValue -> OutValue -> Computation
evalAdd v1 v2 v3 = do
  i1 <- fetchIn v1
  i2 <- fetchIn v2
  dest <- fetchOut v3
  update (dest, i1 + i2)
  incPC 4

evalMul :: InValue -> InValue -> OutValue -> Computation
evalMul v1 v2 v3 = do
  i1 <- fetchIn v1
  i2 <- fetchIn v2
  dest <- fetchOut v3
  update (dest, i1 * i2)
  incPC 4

evalInput :: OutValue -> Computation
evalInput value = do
  stat <- gets status
  case stat of
    Running -> getInput
    Reading -> do
      dest <- fetchOut value
      input <- consume
      update (dest, input)
      incPC 2

evalOutput :: InValue -> Computation
evalOutput val = do
  output <- fetchIn val
  write output
  incPC 2

evalJumpIfTrue :: InValue -> InValue -> Computation
evalJumpIfTrue v1 v2 = do
  cond <- fetchIn v1
  dest <- fetchIn v2
  if cond /= 0
  then setPC dest
  else incPC 3

evalJumpIfFalse :: InValue -> InValue -> Computation
evalJumpIfFalse v1 v2 = do
  cond <- fetchIn v1
  dest <- fetchIn v2
  if cond == 0
  then setPC dest
  else incPC 3

evalLessThan :: InValue -> InValue -> OutValue -> Computation
evalLessThan v1 v2 v3 = do
  i1 <- fetchIn v1
  i2 <- fetchIn v2
  dest <- fetchOut v3
  update (dest, if i1 < i2 then 1 else 0)
  incPC 4

evalEquals :: InValue -> InValue -> OutValue -> Computation
evalEquals v1 v2 v3 = do
  i1 <- fetchIn v1
  i2 <- fetchIn v2
  dest <- fetchOut v3
  update (dest, if i1 == i2 then 1 else 0)
  incPC 4

evalRB :: InValue -> Computation
evalRB val = do
  offset <- fetchIn val
  incRB offset
  incPC 2

evalHalt :: Computation
evalHalt = do
  state <- get
  put (state { status = Halted })

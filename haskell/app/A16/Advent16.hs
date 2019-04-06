{-# LANGUAGE InstanceSigs  #-}
{-# LANGUAGE TupleSections #-}

module A16.Advent16 where

import qualified Control.Monad.Reader         as Reader
import           Data.Bits
import           Data.Char
import           Data.List                    (isPrefixOf)
import           Parsers
import           Text.ParserCombinators.ReadP

-- Represents the CPU state with four registers
type Regs = (Int, Int, Int, Int)

-- Returns the nth register
getReg :: Int -> Regs -> Int
getReg 0 (r0, r1, r2, r3) = r0
getReg 1 (r0, r1, r2, r3) = r1
getReg 2 (r0, r1, r2, r3) = r2
getReg 3 (r0, r1, r2, r3) = r3
getReg _ _                = error "Invalid register"

-- Sets the nth register with the given value
setReg :: Int -> Int -> Regs -> Regs
setReg v 0 (r0, r1, r2, r3) = (v, r1, r2, r3)
setReg v 1 (r0, r1, r2, r3) = (r0, v, r2, r3)
setReg v 2 (r0, r1, r2, r3) = (r0, r1, v, r3)
setReg v 3 (r0, r1, r2, r3) = (r0, r1, r2, v)
setReg _ _ _                = error "Invalid register"

-- Each of our op codes
data Opcode
  = Addr
  | Addi
  | Mulr
  | Muli
  | Banr
  | Bani
  | Borr
  | Bori
  | Setr
  | Seti
  | Gtir
  | Gtri
  | Gtrr
  | Eqir
  | Eqri
  | Eqrr
  deriving (Bounded, Enum)

-- List of all opcodes
allOpcodes = [minBound..maxBound] :: [Opcode]

-- The input parameters for each of the Opcodes
type Params = (Int, Int, Int)

-- Represents a deeper state of the params and registers for convenience.
data DeepState = DeepState
  { a, b, c                :: Int
  , r0, r1, r2, r3         :: Int
  , rA, rB, rC             :: Int
  , set0, set1, set2, set3 :: Int -> Regs
  , setC                   :: Int -> Regs
  }

-- Interprets a Params and Regs as a DeepState
toDS :: Params -> Regs -> DeepState
toDS (a, b, c) regs@(r0, r1, r2, r3) =
  DeepState
    { a = a
    , b = b
    , c = c
    , r0 = r0
    , r1 = r1
    , r2 = r2
    , r3 = r3
    , rA = getReg a regs -- Register indicated by param A
    , rB = getReg b regs -- Register indicated by param B
    , rC = getReg c regs -- Register indicated by param C
    , set0 = (, r1, r2, r3)
    , set1 = (r0, , r2, r3)
    , set2 = (r0, r1, , r3)
    , set3 = (r0, r1, r2, )
    , setC = \v -> setReg v c regs
    }

-- Represents a Bool as a 1 or 0
boolToInt :: Bool -> Int
boolToInt True  = 1
boolToInt False = 0

-- Perform an instruction
instr :: Opcode -> DeepState -> Regs
instr Addr ds = setC ds (rA ds + rB ds)
instr Addi ds = setC ds (rA ds + b ds)
instr Mulr ds = setC ds (rA ds * rB ds)
instr Muli ds = setC ds (rA ds * b ds)
instr Banr ds = setC ds (rA ds .&. rB ds)
instr Bani ds = setC ds (rA ds .&. b ds)
instr Borr ds = setC ds (rA ds .|. rB ds)
instr Bori ds = setC ds (rA ds .|. b ds)
instr Setr ds = setC ds (rA ds)
instr Seti ds = setC ds (a ds)
instr Gtir ds = setC ds (boolToInt $ a ds > rB ds)
instr Gtri ds = setC ds (boolToInt $ rA ds > b ds)
instr Gtrr ds = setC ds (boolToInt $ rA ds > rB ds)
instr Eqir ds = setC ds (boolToInt $ a ds == rB ds)
instr Eqri ds = setC ds (boolToInt $ rA ds == b ds)
instr Eqrr ds = setC ds (boolToInt $ rA ds == rB ds)

-- Represents an example in part A
data ExampleA = ExampleA
  { beforeRegs :: Regs
  , opNum      :: Int
  , params     :: Params
  , afterRegs  :: Regs
  }

-- Parses the input as a single ExampleA
exampleAParser :: ReadP ExampleA
exampleAParser = do
  _ <- string "Before: ["
  beforeRegs <- repeatedNReader 4 ", " intReader
  _ <- string "]\n"
  params <- repeatedNReader 4 " " intReader
  _ <- string "\nAfter:  ["
  afterRegs <- repeatedNReader 4 ", " intReader
  _ <- string "]\n\n"
  return
    ExampleA
      { beforeRegs = (head beforeRegs, beforeRegs !! 1, beforeRegs !! 2, beforeRegs !! 3)
      , opNum = head params
      , params = (params !! 1, params !! 2, params !! 3)
      , afterRegs = (head afterRegs, afterRegs !! 1, afterRegs !! 2, afterRegs !! 3)
      }

-- Returns whether the example conforms to the given opcode
exampleAMatch :: ExampleA -> Opcode -> Bool
exampleAMatch ea op = afterRegs ea == afterRegs'
  where
    afterRegs' = instr op (toDS (params ea) (beforeRegs ea))

partA :: [ExampleA] -> Int
partA = length . filter (>= 3) . map (\ea -> length . filter (exampleAMatch ea) $ allOpcodes)

main :: IO ()
main = do
  input <- getContents
  let (exampleAs, remaining) = last (readP_to_S (many exampleAParser) input)
  print $ partA exampleAs

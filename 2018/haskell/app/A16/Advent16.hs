{-# LANGUAGE InstanceSigs  #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TupleSections #-}

module A16.Advent16 where

import qualified Control.Monad.Reader         as Reader
import           Data.Bits
import           Data.Char
import           Data.List                    (isPrefixOf)
import qualified Data.Map                     as Map
import qualified Data.Set                     as Set
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
  deriving (Bounded, Enum, Eq, Show, Ord)

-- List of all opcodes
allOpcodes = [minBound .. maxBound] :: [Opcode]

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

-- Perform an instruction, returning the value that should be stored in the output register
instr :: Opcode -> DeepState -> Int
instr Addr ds = rA ds + rB ds
instr Addi ds = rA ds + b ds
instr Mulr ds = rA ds * rB ds
instr Muli ds = rA ds * b ds
instr Banr ds = rA ds .&. rB ds
instr Bani ds = rA ds .&. b ds
instr Borr ds = rA ds .|. rB ds
instr Bori ds = rA ds .|. b ds
instr Setr ds = rA ds
instr Seti ds = a ds
instr Gtir ds = boolToInt $ a ds > rB ds
instr Gtri ds = boolToInt $ rA ds > b ds
instr Gtrr ds = boolToInt $ rA ds > rB ds
instr Eqir ds = boolToInt $ a ds == rB ds
instr Eqri ds = boolToInt $ rA ds == b ds
instr Eqrr ds = boolToInt $ rA ds == rB ds

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
    result = instr op (toDS (params ea) (beforeRegs ea)) -- Evaluate operation
    (_, _, c) = params ea -- Destination register
    afterRegs' = setReg result c (beforeRegs ea) -- Store result in destination

-- Solves Problem A, returning the number of examples that match three or more opcodes
partA :: [ExampleA] -> Int
partA = length . filter (>= 3) . map (\ea -> length . filter (exampleAMatch ea) $ allOpcodes)

-- Represents a mapping from Opcodes to the set of Op Ints that can possibly represent that Opcode
type Constraints = Map.Map Opcode (Set.Set Int)

-- Deletes all the items contained in l from the Set
deleteAll ::
     forall a l. (Ord a, Foldable l)
  => l a
  -> Set.Set a
  -> Set.Set a
deleteAll items s = foldl (flip Set.delete) s items

-- Propagate constraints, greedily eliminating candidate Op Ints.
propConstraints :: Constraints -> Constraints
propConstraints c
  | c == c' = c -- Constraint propagation did not yield any pruning
  | otherwise = propConstraints c'
  where
    c' = propConstraintsOnce c
    propConstraintsOnce :: Constraints -> Constraints
    propConstraintsOnce c = Map.map (deleteAll solved) c
      where
        solved = foldl1 Set.union . Map.elems . Map.filter ((== 1) . length) $ c

processExamplesOnce :: [ExampleA] -> Constraints -> Constraints
processExamplesOnce [] c = c
processExamplesOnce (ea:tail) c = propConstraints newConstraints
  where
    elligibleOpcodes = Map.keys . Map.filter (Set.member (opNum ea)) $ c
    failingOpcodes = filter (not . exampleAMatch ea) elligibleOpcodes
    newConstraints = foldl (deleteOpnum (opNum ea)) c failingOpcodes
    deleteOpnum opnum c opcode = Map.update (Just . Set.delete opnum) opcode c

processExamples :: [ExampleA] -> Constraints -> Constraints
processExamples eas c
  | allSolved c = c -- Fully constrained!
  | c == c' = error $ "Not enough constraints, Opnums ambiguous: " ++ show c
  | otherwise = processExamples eas c'
  where
    c' = processExamplesOnce eas c
    allSolved = all ((== 1) . Set.size)

partB :: [ExampleA] -> Constraints
partB eas = processExamples eas startingConstraints
  where
    startingConstraints = Map.fromList . map (, Set.fromList [0 .. 15]) $ allOpcodes


main :: IO ()
main = do
  input <- getContents
  let (exampleAs, remaining) = last (readP_to_S (many exampleAParser) input)
  print $ partA exampleAs
  print $ partB exampleAs

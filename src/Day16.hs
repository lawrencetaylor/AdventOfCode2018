module Day16 where

import Data.Either(fromRight)
import Common
import Text.ParserCombinators.Parsec(Parser, ParseError)
import Text.Parsec(sepBy, newline, string, char, many, anyChar, option) 
import qualified Data.Map as M
import qualified Data.Bits as B
import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.List.Extra as L

type Registers = M.Map Int Int

data OpCode = 
  AddR | AddI
  | MulR | MulI
  | BanR | BanI
  | BorR | BorI
  | SetR | SetI
  | GtIR | GtRI | GtRR
  | EqIR | EqRI | EqRR
  deriving (Show, Eq, Ord)

allCodes =
  [ AddR, AddI 
  , MulR, MulI 
  , BanR, BanI
  , BorR, BorI
  , SetR, SetI
  , GtIR, GtRI, GtRR
  , EqIR, EqRI, EqRR]

type Instruction = (OpCode, Int, Int, Int)

type OpCodeIndex = Int
type UnknownInstruction = (OpCodeIndex, Int, Int, Int)
type Sample =  (Registers, UnknownInstruction, Registers )

pBefore :: Parser [Int]
pBefore = do
  string "Before: ["
  v <- sepBy pInt (string ", ") 
  char ']'
  return v

pTestInstruction :: Parser UnknownInstruction
pTestInstruction = do
  opCodeIndex <- pInt
  char ' '
  a <- pInt
  char ' '
  b <- pInt
  char ' '
  c <- pInt
  return (opCodeIndex, a, b, c)

pAfter :: Parser [Int]
pAfter = do
  string "After:  ["
  v <- sepBy pInt (string ", ")
  char ']'
  return v

toRegister :: [Int] -> Registers
toRegister = M.fromList . zip [0..]

pSample :: Parser Sample
pSample = do
  before <- toRegister <$> pBefore
  newline
  instruction <- pTestInstruction
  newline
  after <- toRegister <$> pAfter
  newline
  newline
  return (before, instruction, after)

pSamples :: Parser [Sample]
pSamples = many pSample

pFile :: Parser ([Sample], [UnknownInstruction])
pFile = do 
  samples <- pSamples
  newline
  newline
  instructions <- sepBy pTestInstruction newline
  return (samples, instructions) 

passes :: Sample -> OpCode -> Bool
passes (before, (_, a, b, c), after) code = 
  (==) after $ eval (code, a, b, c) before

eval :: Instruction -> Registers -> Registers
eval (ins, a, b, c) regs = M.insert c regValue regs
  where 
    reg r = regs M.! r

    regValue =
      case ins of
        AddR -> (reg a) + (reg b)
        AddI -> (reg a) + b
        MulR -> (reg a) * (reg b)
        MulI -> (reg a) * b
        BanR -> (reg a) B..&. (reg b)
        BanI -> (reg a) B..&. b
        BorR -> (reg a) B..|. (reg b)
        BorI -> (reg a) B..|. b
        SetR -> reg a
        SetI -> a
        GtIR -> if (a > reg b) then 1 else 0
        GtRI -> if (reg a > b) then 1 else 0
        GtRR -> if (reg a > reg b) then 1 else 0
        EqIR -> if (a == reg b) then 1 else 0
        EqRI -> if (reg a == b) then 1 else 0
        EqRR -> if (reg a == reg b) then 1 else 0


type OpCodeCandidates = (Int, S.Set OpCode )

candidates :: Sample -> OpCodeCandidates
candidates sample = (i, S.fromList possibleCodes)
  where 
    (_, (i, _,_,_), _) = sample
    possibleCodes = filter (passes sample) allCodes

step :: ([OpCodeCandidates], M.Map Int OpCode) -> ([OpCodeCandidates], M.Map Int OpCode)
step (candidates, map) = (newCandidates, newMap)
  where 
    resolved = 
      fmap (\(i, s) -> (i, S.elemAt 0 s)) $
      filter ((==) 1 . length . snd) candidates

    resolvedCodes = S.fromList $ snd <$> resolved
    newMap = M.union map (M.fromList $ resolved)
    newCandidates = 
      fmap (\(i, c) -> (i, c `S.difference` (S.fromList $ snd <$> resolved ) )) $
      filter (\c -> not $ M.member (fst c) newMap) candidates

opCodes :: [Sample] -> M.Map Int OpCode
opCodes samples = 
  snd $ head $ dropWhile ( not . L.null . fst ) (iterate step initial)
  where 
    initial = (L.nub $ candidates <$> samples, M.empty)

partOne :: [Sample] -> Int
partOne samples = 
  length $ filter condition samples
  where 
    condition sample = (<) 2 . length . filter (passes sample) $ allCodes

instruction :: M.Map Int OpCode -> UnknownInstruction -> Instruction
instruction codes (i, a, b, c) = (codes M.! i, a, b, c)

partTwo :: [Sample] -> [UnknownInstruction] -> Int
partTwo samples unknownInstructions = 
  finalState M.! 0
  where 
    codes = opCodes samples
    instructions = instruction codes <$> unknownInstructions
    iEval a b = eval b a
    finalState = foldl iEval (M.fromList [(0,0), (1,0), (2,0), (3,0)]) instructions

rawData :: IO String
rawData = readData "data\\Day16"

main :: IO ()
main = do
  (samples, program) <- (fromRight ([], []) . parse pFile <$> rawData)

  putStrLn $ "Day 16 [Part 1] = " ++ (show $ partOne samples) 
  putStrLn $ "Day 16 [Part 2] = " ++ (show $ partTwo samples ( program)) 
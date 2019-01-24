module Day19 where

import Data.Either(fromRight)
import Common
import Text.ParserCombinators.Parsec(Parser, ParseError)
import Text.Parsec(sepBy, newline, string, (<|>), choice, char, try) 
import DeviceProgram
import qualified Data.Char as C
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Numbers.Primes as P

pOpCode :: Parser OpCode
pOpCode = 
  choice $
  fmap (\(token, code) -> for token code ) $
  zip ((fmap C.toLower . show) <$> allCodes) allCodes
  where 
    for :: String -> OpCode -> Parser OpCode
    for token code = try (string token *> (return $ code))

pInstruction :: Parser Instruction
pInstruction = do
  opCode <- pOpCode
  a <- char ' ' *> pInt
  b <- char ' ' *> pInt
  c <- char ' ' *> pInt
  return (opCode, a, b, c)

pInitial :: Parser Int
pInitial = string "#ip " *> pInt

pProgram :: Parser Program
pProgram = do
  init <- pInitial
  newline
  ins <- sepBy pInstruction newline
  return (init, ins)

type Program = (BoundRegister, [Instruction])
type BoundRegister = Int
type InstructionPointer = Int
type Regs = (BoundRegister, Registers)

evaluate :: [Instruction] -> Regs -> Regs
evaluate instructions (b, r)  = 
  let 
    ip = r M.! b
    r' = M.insert b ip r
    instruction = instructions !! ip
    r'' = eval instruction r'
    rFinal = M.update (\a -> Just (a+1)) b r''
  in (b, rFinal)

check :: Int -> Regs -> Maybe (Regs, Regs)
check instructionCount regs@(b, r) = 
  case ((r M.! b) >= instructionCount) of
    True -> Nothing
    False -> Just (regs, regs)

evaluateAndCheck :: [Instruction] -> Regs -> Maybe (Regs, Regs)
evaluateAndCheck  instructions regs = 
  let 
    checkRegs = check (length instructions)
    eval = evaluate instructions
  in (checkRegs . eval) regs

display :: Regs -> String
display (_, r) = show $ snd <$> M.toList r

run (boundReg, instructions) = 
  let 
    eval = evaluateAndCheck instructions
    init = (boundReg, M.fromList $ zip [0..5] (repeat 0))
    seq = L.unfoldr eval init
  in 
    seq

zeroFinalValue :: Int -> [Instruction] -> Registers -> Int
zeroFinalValue boundReg instructions regs = 
  let 
    init = (boundReg, regs)
    seq = run (boundReg, instructions)
    (_, finalRegs) = L.last seq
  in finalRegs M.! 0

rawData :: IO String
rawData = readData "data\\Day19"

{-
    Program calculates the sum of the factors of
      955 in Part 1
      10551355 in Part 
      
    S(n) = Sum of factors of n obeys S(n*m) = S(n)*S(m) 
    for n, m coprime

    S(955) = S(5*191) = S(5) * S(191) = 6 * 193 = 1152
    S(10551355) = S(5*499*4229) = 6*500*4230
-}

main :: IO ()
main = do
  (boundReg, instructions) <- (fromRight (0,[]) . parse pProgram <$> rawData)
  let init = M.fromList $ zip [0..5] [0,0,0,0,0,0]
  let p1 = zeroFinalValue boundReg instructions init
  putStrLn $ "Day 19 [Part 1] = " ++ (show p1) 


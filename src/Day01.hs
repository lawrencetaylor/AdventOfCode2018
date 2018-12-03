module Day01 where

import qualified Data.Set as S
import Data.Either
import Data.Sequence(Seq(..))
import Common
import Text.ParserCombinators.Parsec(Parser, ParseError)
import Text.Parsec(many1, digit, sepBy, newline, tab, char, (<|>) )

getRaw :: IO String
getRaw = readData "data\\Day01"

-- Parsing

pSign :: Parser Int
pSign = do
  sign <- (char '+' <|> char '-')
  return $
    case sign of
      '+' -> 1
      '-' -> -1

pLine :: Parser Int
pLine = do 
  multiplier <- pSign
  integer <- pInt
  return $ multiplier * integer

pLines :: Parser [Int]
pLines = sepBy pLine newline

-- Solution

firstDuplicateInner :: [Int] -> S.Set Int -> Maybe Int
firstDuplicateInner (x:xs) found = 
  case S.member x found of
  True -> Just x
  False -> firstDuplicateInner xs (S.insert x found)

firstDuplicate :: [Int] -> Maybe Int
firstDuplicate t = firstDuplicateInner t S.empty

main :: IO ()
main = do
  input <- (fromRight [] . parse pLines) <$> readData "data\\Day01"

  let p1 = foldl (+) 0 input
  putStrLn $ "Day 01 [Part 1] = " ++ (show p1) 
  let p2 = firstDuplicate (scanl (+) 0 (cycle input))
  putStrLn $ "Day 01 [Part 2] = " ++ (show p2) 
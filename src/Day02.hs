module Day02 where

import Common(parse, readData)
import Data.Either(fromRight)
import qualified Data.Text as T
import qualified Data.List as L
import Text.ParserCombinators.Parsec(Parser, ParseError)
import Text.Parsec(many,  sepBy, newline, alphaNum  )
import Control.Monad

-- Parsing

rawData :: IO String
rawData = readData "data\\Day02"

pLine :: Parser String
pLine = many alphaNum 

pLines :: Parser [String]
pLines = sepBy pLine newline

-- Solution

hasCharWithCount :: Int -> String -> Bool
hasCharWithCount n t = 
  any (\s -> True) $
  filter (\s -> (length s) == n) $
  L.group $
  L.sort t

numberWithACharCountOf :: Int ->[String] -> Int
numberWithACharCountOf n = length . filter (hasCharWithCount n)

checkSum :: [String] -> Int
checkSum t = (numberWithACharCountOf 2 t) * (numberWithACharCountOf 3 t)

numberOfDiffs :: String -> String -> (Int, String, String)
numberOfDiffs t1 t2 = 
  (n, t1, t2)
  where 
    n = 
      length $ 
      filter (\(a,b) -> a /= b)  $ 
      zip t1 t2

correctIdPair :: [String] -> (String, String)
correctIdPair t1 =
  head $
  map (\(_, x, y) -> (x, y)) $
  filter (\(a, _, _) -> a == 1) $
  numberOfDiffs <$> t1 <*> t1

commonLetters :: (String, String) -> String
commonLetters (x, y) = 
  map fst $ filter (\(a,b) -> a == b) $ zip x y 

main :: IO ()
main  = do
  input <- (fromRight [] . parse pLines <$> rawData)

  let p1 = checkSum input
  putStrLn $ "Day 02 [Part 1] = " ++ (show p1) 

  let p2 = commonLetters . correctIdPair $ input
  putStrLn $ "Day 02 [Part 2] = " ++ (show p2) 
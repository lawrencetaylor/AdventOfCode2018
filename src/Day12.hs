module Day12 where

import Data.Either(fromRight)
import Common
import Text.ParserCombinators.Parsec(Parser, ParseError, (<|>))
import Text.Parsec(sepBy, newline, string, char, many) 
import qualified Data.Vector as V
import qualified Data.Map as M
import Debug.Trace
import qualified Data.Set as S
import Data.Function.Memoize

-- Parsing

-- initial state: #..#.#..##......###...###

pInitialState :: Parser [Char]
pInitialState = do
  string "initial state: "
  many (char '.' <|> char '#')

-- ...## => #
pLine :: Parser ([Char], Char)
pLine = do
  pots <- many (char '.' <|> char '#')
  string " => "
  to <- (char '.' <|> char '#')
  return $ (pots, to)

pFile :: Parser ([Char], [(String, Char)])
pFile = do
  initialState <- pInitialState
  newline
  newline
  lines <- sepBy pLine newline
  return $ (initialState, lines)

-- Solution

next :: M.Map String Char -> V.Vector Char -> Int -> Char
next m v {-- Vector --} i {-- Index --} = 
  case M.lookup neighbours m of
    Nothing -> '.'
    Just c -> c
  where 
  neighbours = V.toList $ V.slice (i-2) 5 v

-- Drops trailing '.''s, and also returns the 
-- new current starting position of the pots
dropTrailing :: Int -> [Char] -> (Int, [Char])
dropTrailing i ('.':xs) = dropTrailing (i+1) xs
dropTrailing i x = (i, x)

data Gen = Gen {
    pots :: String
  , startingPosition :: Int
  }
  
nextGen :: M.Map String Char -> Gen -> Gen
nextGen m g = Gen trimTrailingEmpty newCurrent
  where 
    vMod = "...." ++ (pots g) ++ "...."
    vSize = length vMod
    newState = 
      (\(i, vect) -> next m (V.fromList vect) i ) <$>  ([2..vSize-3] `zip` (repeat vMod))
    (newCurrent, trimLeadingEmpty) = dropTrailing ((startingPosition g) - 2) newState
    trimTrailingEmpty = reverse $ dropWhile ((==) '.') $ reverse trimLeadingEmpty 

sumPots :: Gen -> Int
sumPots g = 
  sum $ snd <$> (filter ((==) '#'  . fst) $ (pots g) `zip ` [startingPosition g..])

rawData :: IO String
rawData = readData "data\\Day12"

findDuplicatePots :: M.Map String Int -> [(String, Int)] -> (Int, Int)
findDuplicatePots seen ((str, i):xs) =
  case M.lookup str seen of
    (Just j) -> (j, i)
    Nothing -> findDuplicatePots (M.insert str i seen) xs

main :: IO ()
main = do
  input <- (fromRight ([], []) . parse pFile <$> rawData)

  let (initialState, associations) = input

  let state = ".." ++ initialState ++ ".."
  let mappings = M.fromList associations

  let iter = nextGen mappings
  let x =  iterate iter $ Gen initialState 0

  putStrLn $ "Day 12 [Part 1] = " ++ (show $ sumPots (x !! 20)) 

-- We find the pattern of pots duplicates it's self from generation 
-- 96 to 97, with an increment in the starting position of 1.  By eye-balling
-- the values we can see that from this point on the starting position at 
-- generation i = i - 72
  let y@(firstDup,_) = findDuplicatePots M.empty ((pots <$> x) `zip` [0..])
  putStrLn $ "Duplicate Pots @ generations " ++ (show y)
  let eventualPots = pots $ x !! firstDup
  putStrLn $ "Day 12 [Part 1] = " ++ (show $ sumPots (Gen eventualPots (50000000000 - 72))) 
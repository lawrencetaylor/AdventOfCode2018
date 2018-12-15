module Day14 where

import Data.Either(fromRight)
import Common
import Text.ParserCombinators.Parsec(Parser, ParseError)
import Text.Parsec(sepBy, newline, string) 
import qualified Data.Map as M
import qualified Data.Char as C
import qualified Data.Vector as V
import qualified Data.Sequence as S
import qualified Data.List as L

type Recipe = Int
type Elf  = Int

inner :: S.Seq Recipe -> [Elf] -> ([Recipe], [Elf])
inner previous elves = (addedRecipes, newIndices)
  where
    addedRecipes = fmap C.digitToInt $ show $ sum $ map (\i -> previous `S.index` i) elves
    newRecipes = previous S.>< S.fromList addedRecipes
    newCurrent i = (1 + i + (newRecipes `S.index` i)) `mod` (length newRecipes)
    newIndices = newCurrent <$> elves

recipes :: [Recipe]
recipes = 3 : 7 : go (S.fromList [3,7]) [0,1]
  where
    go s elves = newRecipes ++ go (s S.>< (S.fromList newRecipes)) newElves
     where 
      (newRecipes, newElves) = inner s elves

partOne :: Int -> String
partOne count =  take 10 $ drop count $ fmap C.intToDigit recipes

partTwo ::  String -> Int -> String -> Int
partTwo str counter recipes = 
  L.length $ 
  L.takeWhile (\x -> not $ all (\(x,y) -> x == y) $ L.zip str x) $
  L.tails recipes

main :: IO ()
main = do

  putStrLn $ "Day 14 [Part 1] = " ++ (show $ partOne 084601)
  putStrLn $ "Day 14 [Part 2] = " ++ (show $ partTwo "084601" 0 (fmap C.intToDigit recipes))  
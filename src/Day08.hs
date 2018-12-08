module Day08 where

import Data.Either(fromRight)
import Common
import Text.ParserCombinators.Parsec(Parser, ParseError)
import Text.Parsec(sepBy, newline, string, char, count, option, space) 
import qualified Data.Vector as V
import Data.Vector((!))
import qualified Data.Tree as T
import qualified Data.Map as M

rawData :: IO String
rawData = readData "data\\Day08"

pTree :: Parser (T.Tree [Int])
pTree = do
  childCount <- pInt
  char ' '
  metaCount <- pInt
  char ' '
  children <- count childCount pTree
  meta <- count metaCount (pInt <* option ' ' space)
  return $ T.Node meta children
  
sumMeta :: T.Tree [Int] -> Int
sumMeta (T.Node meta children) = sum meta + childrenMeta
  where 
    childrenMeta = sum $ sumMeta <$> children

tValue :: T.Tree [Int] -> Int
tValue (T.Node meta []) = sum meta
tValue (T.Node meta c ) = sum $ getValue <$> meta
  where
    indexedChildren = M.fromList $ zip [1..] c
    getValue m = maybe 0 id $ tValue <$> M.lookup m indexedChildren
    
partOne :: T.Tree [Int] -> [Int]
partOne = undefined

partTwo :: String -> String
partTwo = id

main :: IO ()
main = do
  input <- (fromRight (T.Node [] [] ) . parse pTree <$> rawData)

  let p1 = sumMeta input
  putStrLn $ "Day 08 [Part 1] = " ++ (show p1) 

  let p2 = tValue input
  putStrLn $ "Day 08 [Part 2] = " ++ (show p2) 
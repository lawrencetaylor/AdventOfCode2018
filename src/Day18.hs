{-# Language TupleSections #-}
module Day18 where

import Data.Either(fromRight)
import Common
import Text.ParserCombinators.Parsec(Parser, ParseError)
import Text.Parsec(sepBy, newline, string) 
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Maybe as Maybe

data Acre = Open | Tree | Yard deriving (Eq, Ord)
data Coord = Coord (Int, Int) deriving (Ord, Show, Eq)
type Landscape = M.Map Coord Acre

isTree :: Acre -> Bool
isTree Tree = True
isTree _ = False

isOpen :: Acre -> Bool
isOpen Open = True
isOpen _ = False

isYard :: Acre -> Bool
isYard Yard = True
isYard _ = False

pRow :: String -> [(Int, Acre)]
pRow str = 
  let 
    toAcre '.' = Open
    toAcre '#' = Yard
    toAcre '|' = Tree
  in fmap toAcre <$> zip [0..] str

pRows :: [String] -> Landscape
pRows rows =
  M.fromList 
    $ [ (Coord (x,y), acre) | 
        (y, row) <- zip [0..] rows
      , (x, acre) <- pRow row]

instance Semigroup Coord where
  Coord (x,y) <> Coord (a,b) = Coord (x+a, y+b)

xCoord :: Coord -> Int
xCoord (Coord (x,y)) = x

yCoord :: Coord -> Int
yCoord (Coord (x,y)) = y

adjacent :: Landscape -> Coord -> [Acre]
adjacent landscape coord =
    Maybe.catMaybes $
    fmap (\a -> M.lookup a landscape) $
    filter ((/=) coord) $
    fmap ((<>) coord) $
    fmap Coord $
    (,) <$> 
    [-1,0,1] <*> [-1,0,1]

next :: Landscape -> Coord -> Acre
next landscape coord 
  | (isOpen $ landscape M.! coord) && (enclosedBy 3 isTree) = Tree
  | (isTree $ landscape M.! coord) && (enclosedBy 3 isYard) = Yard
  | (isYard $ landscape M.! coord) 
      && (enclosedBy 1 isYard) 
      && (enclosedBy 1 isTree) = Yard
  | (isYard $ landscape M.! coord) = Open
  | otherwise = landscape M.! coord
  where 
    enclosedBy n predicate = 
      (<=) n $
      length $ 
      filter predicate $ 
      (adjacent landscape coord)

nextLandscape :: Landscape -> Landscape
nextLandscape landscape = 
  M.fromList $
  fmap (\k -> (k, next landscape k)) $
  M.keys landscape 

testData = [ 
    ".#.#...|#."
  , ".....#|##|"
  , ".|..|...#."
  , "..|#.....#"
  , "#.#|||#|#|"
  , "...#.||..."
  , ".|....|..."
  , "||...#|.#|"
  , "|.||||..|."
  , "...#.|..|." ]

display :: Landscape -> String
display landscape = 
  let 
    maxX = maximum $ 
              xCoord <$> 
              M.keys landscape
    maxY = maximum $ 
              yCoord <$> 
              M.keys landscape

    toChar Tree = '|'
    toChar Open = '.'
    toChar Yard = '#'

    row y = [ toChar $ landscape M.! (Coord (x,y)) | x <- [0..maxY] ]
  in 
    concatMap id $
    L.intersperse "\n" $
    row <$>
    [0..maxY]

afterMinutes :: Int -> Landscape -> Landscape
afterMinutes n landscape =
  head $
  head $ 
  drop n $
  L.tails $
  iterate nextLandscape $ 
  landscape 

resourceValue landscape = 
  let 
    treeCount = length $ M.filter isTree landscape
    yardCount = length $ M.filter isYard landscape
  in treeCount * yardCount


rawData :: IO String
rawData = readData "data\\Day18"

partOne :: Int -> Landscape -> Int
partOne mins = resourceValue . afterMinutes mins

repeatingBoundary :: Landscape -> (Int, Int)
repeatingBoundary landscape = firstDuplicate indexedSeq M.empty
  where 
    indexedSeq = (take 500 $ (iterate nextLandscape) landscape) `zip` [0..]

firstDuplicate :: [(Landscape, Int)] -> M.Map Landscape Int -> (Int, Int)
firstDuplicate ((l, i):xs) seen
    | M.member l seen = (seen M.! l, i)
    | otherwise = firstDuplicate xs (M.insert l i seen)

main :: IO ()
main = do
  landscape <- (pRows . lines <$> rawData)

  let p1 = partOne 10 landscape
  putStrLn $ "Day 18 [Part 1] = " ++ (show p1) 

  {-
  let (lower, upper) = repeatingBoundary landscape
  (lower, upper) = (451, 479)
  1,000,000,000 = 451 + n * (479 - 451) + r
  r = 17
  Need to calculate resource value after 451 + 17 = 468 mins
  -}

  let p2 = partOne 468 landscape
  putStrLn $ "Day 18 [Part 2] = " ++ (show p2) 


module Day06 where

import Data.Either(fromRight)
import Common
import Text.ParserCombinators.Parsec(Parser, ParseError)
import Text.Parsec(sepBy, newline, string)
import qualified Data.List as L
import qualified Data.Map as M

pPoint :: Parser (Int, Int)
pPoint = do
  x <- pInt
  string ", "
  y <- pInt
  return $ (x, y)

pPoints :: Parser [(Int, Int)]
pPoints = sepBy pPoint newline

rawData :: IO String
rawData = readData "data\\Day06"

-- Solution
type Point = (Int, Int)
type State = M.Map Point Int

manhattan :: Point -> Point -> Int
manhattan (x1, y1) (x2,y2) = abs (x1 - x2) + abs (y1 - y2)

bounds :: Int -> [Point] -> [Point]
bounds wiggle p =
  [(x,y) | x <- [minX..maxX], y <- [minY..maxY]] 
    where 
      maxX = (maximum $ fst <$> p) + wiggle
      minX = (minimum $ fst <$> p) - wiggle
      maxY = (maximum $ snd <$> p) + wiggle
      minY = (minimum $ snd <$> p) - wiggle

closestPoint :: [Point] -> Point -> Maybe Point
closestPoint pts p = r
  where 
    w = (\(x,y) -> (y, manhattan x y))  <$> (,) p <$> pts
    y = L.sortOn snd w
    z = L.groupBy (\p1 p2 -> (==) (snd p1) (snd p2)) y
    c = head z
    r = case c of 
      [(x, _)] -> Just x
      otherwise -> Nothing

increment :: Maybe Int -> Maybe Int
increment Nothing = Just 1
increment (Just a) = Just $ a + 1

foldState :: [Point] -> Point -> State -> State
foldState pts p s = 
  case closestPoint pts p of
    Nothing -> s
    Just pt -> M.alter increment pt s

findAreas :: [Point] -> Int -> State
findAreas pts n = L.foldr (foldState pts) M.empty (bounds n pts)

partOne :: [Point] -> Int
partOne pts = y
  where 
    -- We need to check that the size of the areas don't change when we 
    -- wiggle them (e.g change 0 to 1)  to ensure the areas are not infinite. 
    -- The values 0 and 1 were foundby trial and error. 
    maxFiniteAreaWithWiggle = findAreas pts
    s1 = maxFiniteAreaWithWiggle 0
    s2 = maxFiniteAreaWithWiggle 1
    y = maximum $ snd <$> fst <$> (filter (\((_, a), (_, b)) -> a == b) $ zip (M.toList s1) (M.toList s2))

partTwo :: [Point] -> Int
partTwo pts = safePoints
  where 
    -- Again - the value 0 was found by trial and error to ensure
    -- the size of the safe area did not grow
    safePoints = length $ filter isSafe $ bounds 0 pts
    isSafe p = (sum $ manhattan p <$> pts) < 10000

main :: IO ()
main = do
  input <- (fromRight [] . parse pPoints <$> rawData)

  let p1 = partOne $ input
  putStrLn $ "Day 06 [Part 1] = " ++ (show p1) 

  let p2 = partTwo input
  putStrLn $ "Day 06 [Part 2] = " ++ (show p2) 